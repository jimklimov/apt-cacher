#! /usr/bin/perl
#
# lib/apt-cacher-cs.pl
#
# Library file for apt-cacher with checksum specific common code

use strict;
use warnings;

# Handle signals so that END blocks get executed
use sigtrap qw(handler sig_handler normal-signals error-signals);

use BerkeleyDB ();
use Digest::SHA;
use Digest::MD5;
use IO::Uncompress::AnyUncompress qw($AnyUncompressError);
use Fcntl qw(O_RDONLY O_CREAT :flock);
use IPC::SysV qw(IPC_CREAT IPC_EXCL SEM_UNDO);
use IPC::Semaphore;

our $cfg;

BEGIN {
    foreach (\&db, \&import_sums, \&check_sum) { # Silence redefintion warning
        undef &$_;
    }
}

sub sig_handler {
    warn "Got SIG@_. Exiting gracefully!\n" if $cfg->{debug};
    exit 1;
}

sub env {
    my ($nolock) = @_;

    debug_message('Init checksum database environment') if defined &debug_message && $cfg->{debug};

    # Serialise enviroment handling
    my $envlock;
    unless ($nolock) {
	sysopen($envlock, "$cfg->{cache_dir}/private/dbenvlock", O_RDONLY|O_CREAT) ||
	  die "Unable to open DB environment lockfile: $!\n";
	_flock($envlock, LOCK_EX)|| die "Unable to lock DB environment: $!\n";
    }

    my @envargs = (
		   -Home   => $cfg->{cache_dir},
		   -Flags => BerkeleyDB->DB_CREATE | BerkeleyDB->DB_INIT_MPOOL | BerkeleyDB->DB_INIT_CDB,
		   -ThreadCount => 64
		  );

    my $logfile;
    push (@envargs, (-ErrFile => $logfile, -ErrPrefix => localtime . " [$$]")) if open($logfile, '>>', "$cfg->{log_dir}/db.log");
    debug_message('Create DB environment') if defined &debug_message && $cfg->{debug};
    my $env;
    $env=BerkeleyDB::Env->new(@envargs)
      or $nolock # Only if we have locked
	or do {
	    warn "Failed to create DB environment: $BerkeleyDB::Error. Attempting recovery...\n";
	    db_recover();
	    $env = BerkeleyDB::Env->new(@envargs);
	} ;
    die "Unable to create DB environment: $BerkeleyDB::Error\n" unless $env;

    # Set environment lock timeout
    $env->set_timeout($cfg->{request_timeout}*10**6, BerkeleyDB->DB_SET_LOCK_TIMEOUT); # in µs

    $env->set_isalive;
    if (failchk($env) == BerkeleyDB->DB_RUNRECOVERY) {
	# Notify all processes using the Environment
	$env->set_flags(BerkeleyDB->DB_PANIC_ENVIRONMENT, 1);
	die "DB environment requires recovery but not holding lock. Exiting.\n" if $nolock;
	warn "Failed thread detected. Running database recovery.\n";
	db_recover();
	$env = BerkeleyDB::Env->new(@envargs)
	  or die "Unable to recreate DB environment: $BerkeleyDB::Error\n";
    }

    unless ($nolock) {
	_flock($envlock, LOCK_UN)||die "Unable to unlock DB environment: $!\n";
        close($envlock);
    }

    return $env;
}

# Returns a DB handle
#
# Note: BerkeleyDB is not reentrant/fork safe, so avoid forking or calling this
# function whilst a previously returned handle is still in scope.
sub db {
    my $dbfile="$cfg->{cache_dir}/sums.db";
    debug_message('Init checksum database') if defined &debug_message && $cfg->{debug};

    debug_message('Open database') if defined &debug_message && $cfg->{debug};
    my $dbh = BerkeleyDB::Btree->new(-Filename => $dbfile,
				     -Flags => BerkeleyDB->DB_CREATE,
				     -Env => env())
      or die "Unable to open DB file, $dbfile $BerkeleyDB::Error\n";

    return $dbh;
}

# Arg is not undef for BerkeleyDB->DB_WRITECURSOR
sub db_cursor {
    my ($write)=@_;
    my $cursor = db()->db_cursor($write?BerkeleyDB->DB_WRITECURSOR:undef) or die $BerkeleyDB::Error;
    return $cursor;
}

# Arg is cursor
# Arg is key reference
# Arg is data reference
sub cursor_next {
    my ($cursor,$keyref,$dataref) = @_;
    return $cursor->c_get($$keyref, $$dataref, BerkeleyDB->DB_NEXT)
}

# Arg is the environment object
sub failchk {
    my ($e) = @_;
    # Sometimes failchk is returning EINVAL (22), so just loop until we get no
    # error or DB_RUNRECOVERY
    while (my $status = $e->failchk) {
	warn "failchk returned $status\n";
	return $status if $status == BerkeleyDB->DB_RUNRECOVERY;
    }
    return 0;
}

sub db_recover {
    env_remove();

    # Verify
    my $dbfile = "$cfg->{cache_dir}/sums.db";
    if (db_verify($dbfile) == 0) {
	warn "Database verification passed.\n";
    }
    else {
	warn 'Database verification failed: ' . db_error() . "\n Moving $dbfile out of the way.\n";
	rename $dbfile, "$dbfile.corrupt";
    }
    return;
}

sub env_remove {
    return unlink <$$cfg{cache_dir}/__db.*>; # Remove environment
}

sub db_error {
    return $BerkeleyDB::Error;
}

sub db_verify {
    my ($file,$env) = @_;
    return BerkeleyDB::db_verify (-Filename=>$file, -Env=>$env);
}

# Returns reference to status and hash of compaction data
# Arg: DB handle ref
sub _db_compact {
    my ($dbh) = @_;
    my %hash;
    my $status;
    return (\'DB not initialised in _db_compact', undef) unless $dbh;
    $hash{compact_timeout} = 10; # microseconds
    my $mode = ($BerkeleyDB::db_version >= 5 || $dbh->type == BerkeleyDB->DB_BTREE) ? BerkeleyDB->DB_FREE_SPACE : BerkeleyDB->DB_FREELIST_ONLY;
    $status = $dbh->compact(undef,undef,\%hash,$mode);

    return [$status, %hash];
}

sub get_sem {
    my ($no_create) = @_; # If set, don't create, only return exisiting segment

    my $key = IPC::SysV::ftok("$cfg->{cache_dir}/sums.db", 1);
    my $sem;
    # First try to create new segment
    if (!$no_create && ($sem = IPC::Semaphore->new($key, 1, oct(666) | IPC_CREAT | IPC_EXCL))) {
	$sem->setall($cfg->{concurrent_import_limit});
    }
    else {
	# Use existing
	$sem = IPC::Semaphore->new($key, 1, oct(666));
    }
    return $sem;
}

sub get_existing_sem {
    return get_sem(1);
}

# arg: name with optional filehandle to be scanned and added to DB
sub import_sums {
    my ($name, $fh) = @_;
    my $sem;
    my $ret = 0;
    return $ret unless $cfg->{checksum};
    if ($cfg->{concurrent_import_limit}) {
	if ($sem = get_sem()) {
	    # Take semaphore
	    $sem->op(0, -1, SEM_UNDO);
	}
	else {
	    warn "Failed to get IPC::Semaphore: $!";
	}
    }

    $cfg->{debug} && debug_message("import_sums(): Waiting to read-lock file '$name' (".filename_fh($fh).")");
    _flock($fh, LOCK_SH) || warn "import_sums(): Unable to get reading lock on '$name': $!";

    tie my %db, "BerkeleyDB::Btree",
      -Filename => "$cfg->{cache_dir}/sums.db",
      -Env => env(),
      -Flags => BerkeleyDB->DB_CREATE
      or die "Failed to tie hash to database: $BerkeleyDB::Error\n";

    if (extract_sums($name, $fh, \%db)) {
        my $count_sums = keys %db;
        $cfg->{debug} && debug_message("import_sums(): call to extract_sums() returned ok; got $count_sums values");
        $ret = 1;
    } else {
        $cfg->{debug} && debug_message("import_sums(): call to extract_sums() failed");
    }

    untie %db;
    _flock($fh, LOCK_UN) || warn "import_sums(): Unable to release lock: $!";

    # Release semaphore
    $sem->op(0, 1, SEM_UNDO) if $sem;

    return $ret;
}

# arg: name
# arg: filehandle
# arg: DB handle
# Returns:
#  0	for failure
#  1	for success/ignore or
#  2	for missing checksum entry (maybe success for some uses)
#  3	some checksum value found in DB, but not for known algorithms
sub check_sum {
    my ($name, $fh) = @_;
    return 1 unless $cfg->{checksum};

    if (ref $fh ne 'GLOB') {
	warn "Not a filehandle";
	return 1;
    }

    unless ($name) {
	info_message('Empty filename in check_sum()');
	return 1; # Ignore
    }

    seek($fh,0,0); # Rewind
    if ($name =~ /2\d{3}-\d{2}-\d{2}-\d{4}\.\d{2}\.gz$/) { # pdiffs need decompressing
	$fh = IO::Uncompress::AnyUncompress->new($fh)
	  or die "Decompression failed: $AnyUncompressError\n";
    }

    my $data;
    my $dbh = db();
    if (my $status = $dbh->db_get($name, $data) != 0) { # Returns 0 on success.
	$cfg->{debug} && debug_message("db_get for $name failed: $status ($BerkeleyDB::Error)");
#	return 1;
	# Checksum is not in cache - not a mismatch, we return nonzero success
	return 2;
    }

    my $href = hashify(\$data);
    foreach (qw/sha1 md5 sha256/) { # Try algorithms in order
        if($href->{$_}) {
	    # now check file
	    my $digest;
	    if (/^sha(\d+)/) {
		$digest = Digest::SHA->new($1)->addfile($fh)->hexdigest;
	    }
	    else {
		$digest = Digest::MD5->new->addfile($fh)->hexdigest;
	    }
	    $cfg->{debug} && debug_message("Verify $name $_: db $href->{$_}, file $digest");
	    $cfg->{debug} && debug_message("Verify $name $_: db $href->{$_}, file $digest: CKSUM_" . ( ($href->{$_} eq $digest) ? "OK" : "MISMATCH" ) );
	    return ($href->{$_} eq $digest);
	}
    }
    $cfg->{debug} && debug_message("No stored checksum found for $name. Ignoring");
#    return 1;
    ### Perhaps some checksums existed, but of unsupported type?..
    return 3;
}

1;
