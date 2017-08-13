#! /usr/bin/perl
#
# lib/apt-cacher-cs.pl
#
# Library file for apt-cacher with checksum specific common code

use strict;
use warnings;

use BerkeleyDB;
use Digest::SHA;
use Digest::MD5;
use IO::Uncompress::AnyUncompress qw($AnyUncompressError);
use Fcntl qw(:DEFAULT :flock);
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

# Need to handle non-catastrophic signals so that END blocks get executed
local $SIG{$_} = \&sig_handler foreach qw{INT TERM PIPE QUIT HUP SEGV};

# Returns a DB handle
#
# Note: BerkeleyDB is not reentrant/fork safe, so avoid forking or calling this
# function time whilst a previously returned handle is still in scope.
sub db {
    my ($nolock) = @_;
    my $dbfile="$cfg->{cache_dir}/sums.db";
    debug_message('Init checksum database') if defined &debug_message && $cfg->{debug};

    # Serialise enviroment handling
    my $envlock;
    unless ($nolock) {
	sysopen($envlock, "$cfg->{cache_dir}/private/dbenvlock", O_RDONLY|O_CREAT) ||
	  die "Unable to open DB environment lockfile: $!\n";
	_flock($envlock, LOCK_EX)|| die "Unable to lock DB environment: $!\n";
    }

    my @envargs = (
		   -Home   => $cfg->{cache_dir},
		   -Flags => DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB,
		   -ThreadCount => 64
		  );

    my $logfile;
    push (@envargs, (-ErrFile => $logfile, -ErrPrefix => "[$$]")) if open($logfile, '>>', "$cfg->{log_dir}/db.log");
    debug_message('Create DB environment') if defined &debug_message && $cfg->{debug};
    my $env;
    eval {
	local $SIG{__DIE__} = 'IGNORE'; # Prevent log verbosity
	local $SIG{ALRM} = sub { die "timeout\n" }; # NB: \n required
	alarm $cfg->{request_timeout};
	$env = BerkeleyDB::Env->new(@envargs);
	alarm 0;
    };
    if ($@) {
	die unless $@ eq "timeout\n"; # propagate unexpected errors
    }
    unless ($env) {
	warn "Failed to create DB environment: $BerkeleyDB::Error. Attempting recovery...\n";
	db_recover();
	$env = BerkeleyDB::Env->new(@envargs);
    }
    die "Unable to create DB environment: $BerkeleyDB::Error\n" unless $env;

    $env->set_isalive;
    failchk($env);

    # Take shared lock. This protects verify which requests LOCK_EX
    db_flock(LOCK_SH)|| die "Shared lock failed: $!\n";

    unless ($nolock) {
	_flock($envlock, LOCK_UN)||die "Unable to unlock DB environment: $!\n";
        close($envlock);
    }

    debug_message('Open database') if defined &debug_message && $cfg->{debug};
    my $dbh = BerkeleyDB::Btree->new(-Filename => $dbfile,
				     -Flags => DB_CREATE,
				     -Env => $env)
      or die "Unable to open DB file, $dbfile $BerkeleyDB::Error\n";

    return $dbh;
}

# Arg is DB handle
# Arg is not undef for DB_WRITECURSOR
sub get_cursor {
    my ($dbh,$write)=@_;
    my $cursor = $dbh->db_cursor($write?DB_WRITECURSOR:undef) or die $BerkeleyDB::Error;
    return $cursor;
}

# Arg is cursor
# Arg is key reference
# Arg is data reference
sub cursor_next {
    my ($cursor,$keyref,$dataref) = @_;
    return $cursor->c_get($$keyref, $$dataref, DB_NEXT)
}

# Arg is the environment object
sub failchk {
    my ($e) = @_;
#    warn "$$ failchk on $e\n";
    if ($e->failchk == DB_RUNRECOVERY) {
	warn "Failed thread detected. Running database recovery\n";
	db_recover();
    }
    return;
}

# Arg is flock flags
my $dblock;
sub db_flock {
    my ($flags) = @_;
    if (!$dblock){
	sysopen($dblock, "$cfg->{cache_dir}/private/dblock", O_RDONLY|O_CREAT) ||
	  die "Unable to open lockfile: $!\n";
    }
    return _flock($dblock, $flags);
}

sub db_recover {
    env_remove();
    my @envargs = (
		   -Home   => $cfg->{cache_dir},
		   -Flags  => DB_CREATE | DB_INIT_LOG |
		   DB_INIT_MPOOL | DB_INIT_TXN |
		   DB_RECOVER | DB_PRIVATE | DB_USE_ENVIRON
		  );

    # Avoid leaving DB log on filesystem if possible
    if ($BerkeleyDB::db_version <= 4.6) {
	# Cannot use for db4.7. Requires log_set_config() to be called before Env open
	eval {push(@envargs, (-SetFlags => DB_LOG_INMEMORY))};
    }
    elsif ($BerkeleyDB::VERSION >= 0.40) {
	eval {push(@envargs, (-LogConfig => DB_LOG_IN_MEMORY))}
    }

    my $logfile;
    push(@envargs, (-ErrFile => $logfile)) if open($logfile, '>>', "$cfg->{log_dir}/db.log");
    my $renv = BerkeleyDB::Env->new(@envargs)
      or die "Unable to create recovery environment: $BerkeleyDB::Error\n";
    unlink "$cfg->{cache_dir}/private/dblock";
    return defined $renv;
}

sub env_remove {
    return unlink <$$cfg{cache_dir}/__db.*>; # Remove environment
}

sub temp_env {
    # From db_verify.c
    # Return an unlocked environment
    # First try to attach to an existing MPOOL
    my $tempenv;
    $tempenv = BerkeleyDB::Env->new(-Home   => $cfg->{cache_dir},
				    -Flags => DB_INIT_MPOOL | DB_USE_ENVIRON)
      or
	# Else create a private region
	$tempenv = BerkeleyDB::Env->new(-Home   => $cfg->{cache_dir},
					-Flags => DB_CREATE | DB_INIT_MPOOL |
					DB_USE_ENVIRON | DB_PRIVATE)
	  or die "Unable to create temporary DB environment: $BerkeleyDB::Error\n";
    return $tempenv;
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
  SWITCH:
    for ($dbh->type) {
	/1/ && do { # Btree
	    $status = $dbh->compact(undef,undef,\%hash,DB_FREE_SPACE);
	    last SWITCH;
	};
	/2/ && do { # Hash
	    $status = $dbh->compact(undef,undef,\%hash,DB_FREELIST_ONLY);
	    last SWITCH;
	};
    }
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
    my %temp;
    my $sem;
    my $ret = 0;
    return unless $cfg->{checksum};
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
    _flock($fh, LOCK_SH) || warn "import_sums(): Unable to get reading lock: $!";
    if (extract_sums($name, $fh, \%temp)) {
	my $dbh = db();
	my $count_ok = 0;
	my $count_total = 0;
	while (my ($filename,$data) = each %temp){
	    $count_total++;
	    if ( $dbh->db_put($filename,$data) == 0 ) {
		$count_ok++;
	    } else {
		warn "db_put $filename, $data failed with $BerkeleyDB::Error" ;
	    }
	}
	$cfg->{debug} && debug_message("import_sums(): call to extract_sums() returned $count_total values, $count_ok were successfully saved to database");
	if ($count_total && $count_ok) {
	    $ret = 1;	### Are zero-counts okay?..
	}
    } else {
	$cfg->{debug} && debug_message("import_sums(): call to extract_sums() failed");
    }
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
	    $cfg->{debug} && debug_message("Verify $name $_: db $href->{$_}, file $digest: CKSUM_" . ( ($href->{$_} eq $digest) ? "OK" : "MISMATCH" ) );
	    return ($href->{$_} eq $digest);
	}
    }
    $cfg->{debug} && debug_message("No stored checksum found for $name. Ignoring");
    ### Perhaps some checksums existed, but of unsupported type?..
    return 3;
}

1;
