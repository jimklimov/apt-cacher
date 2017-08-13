#!/usr/bin/perl

# apt-cacher-cleanup.pl
#
# Script to clean the apt-cacher cache.
#
# Copyright (C) 2007-11, Mark Hindley <mark@hindley.org.uk>
# Copyright (C) 2005, Eduard Bloch <blade@debian.org>
# Copyright (C) 2002-03, Jonathan Oxer <jon@debian.org>
# Portions  (C) 2002, Jacob Lundberg <jacob@chaos2.org>
#
# Distributed under the terms of the GNU Public Licence (GPL).

use strict;
use warnings;
use lib '/usr/share/apt-cacher/lib';

use sigtrap qw(die normal-signals);
use Cwd ();
use Fcntl qw/:DEFAULT :flock F_SETFD/;
use Getopt::Long qw(:config no_ignore_case);
use Digest::SHA;
use HTTP::Date;
use HTTP::Response;
use IO::Uncompress::AnyUncompress qw($AnyUncompressError);
use IO::Compress::Bzip2;
use IO::Compress::Gzip;

my $configfile = '/etc/apt-cacher/apt-cacher.conf';
my $nice_mode=0;
my $verbose=0;
my $help;
my $force;
my $sim_mode=0;
my $offline=0;
my $pdiff_mode=0;
my $db_recover=0;
my @db_mode;
my $patchprog = 'red -s';

my %options = (
    "h|help" => \$help,
    "n|nice" => \$nice_mode,
    "v|verbose" => \$verbose,
    "f|force" => \$force,
    "c|cfg|conf=s" => \$configfile,
    "s|simulate" => \$sim_mode,
    "o|offline" => \$offline,
    "p|pdiff" => \$pdiff_mode,
    "r|recover" => \$db_recover,
    "d|db=s{,}" => \@db_mode
);

{ # Scoping block
    local @ARGV = @ARGV; # Use a local copy in case required for rexec
    if (!GetOptions(%options) || $help) {
	die <<EOM
Usage: $0  [-c|cfg|conf <configfile>] [-n|nice] [-s|simulate]
 [-v|verbose] [-o|offline] [-f|force] [-r|recover] ][-d|db <command> [<arg>]]

Options:
 -n	Renice (and ionice, if possible) to lowest priority and continue.
 -s	Simulate and just show what would be done.
 -o	Don't update index files. Overrides offline_mode from configfile.
 -p	Try to update index files by patching.
 -v	Verbose.
 -f	Force execution, disable sanity checks.
 -r	Attempt recovery of corrupt checksum database.

 -d <command> [<arg>]	Manipulate checksum database.
    Available commands are:
	dump		print db contents
	search <arg>	print entries matching regexp
	delete <arg>	delete entries matching regexp
	import		read new checksum data from Packages and Sources files in cache_dir
	compact		clean and compact database
	failcheck       remove locks left by failed process
        verify		verify the database
EOM
    }
}

if ($sim_mode) {
  $verbose = 1;
  print "Simulation mode. Just printing what would be done.\n";
}

#############################################################################
### configuration ###########################################################
# Include the library for the config file parser
require('apt-cacher.pl');
# Read in the config file and set the necessary variables

# $cfg needs to be global for setup_ownership
our $cfg = eval{ read_config($configfile) };

# not sure what to do if we can't read the config file...
die "Could not read configuration file '$configfile': $@" if $@;

private_config();

# check whether we're actually meant to clean the cache
if ( $cfg->{clean_cache} ne 1 ) {
    printmsg("Maintenance disallowed by configuration item clean_cache\n");
    exit 0;
}

# change uid and gid if root and another user/group configured
if (($cfg->{user} && $cfg->{user} !~ 'root' && !$> )
    || ($cfg->{group} && $cfg->{group} !~ 'root' && !$) =~ /^0/)){
    printmsg("Invoked as root, changing to $cfg->{user}:$cfg->{group} and re-execing.\n");
    setup_ownership($cfg);
    # Rexec to ensure /proc/self/fd ownerships correct which are needed for red
    # patching with pdiffs
    exec($0, @ARGV) or die "Unable to rexec $0: $!\n";
}
# Output data as soon as we print it
local $| = 1;

load_checksum(); # Will disable checksum if BerkeleyDB not available

if ($nice_mode) {
    printmsg("Nice mode\n");
    setpriority 0, 0, 20;
    use Linux::IO_Prio qw(:all);
    ioprio_set(IOPRIO_WHO_PROCESS, $$,IOPRIO_PRIO_VALUE(IOPRIO_CLASS_IDLE, 0)) == 0 ||
      printmsg("ioprio_set failed: $!\n");
}

sub printmsg {
    my @args = @_;
    my $ret;
    $ret = print @args if $verbose;
    return $ret;
}

sub open_lock {
    my ($file) = @_;

    #  Lock header LOCK_EX first to block apt-cacher from working on it
    open(my $hfh, '+<', "../headers/$file") || do {
	warn ("Error: cannot open ../headers/$file for locking: $!\n");
	return;
    };
    _flock($hfh, LOCK_EX) || die "Lock ../headers/$file failed: $!";

    # Read lock file LOCK_SH
    open(my $cfh, '+<', $file) || do {
	warn ("Error: cannot open $file for locking: $!\n");
	return;
    };
    _flock($cfh, LOCK_SH) || die "Lock $file failed: $!";

    return ($cfh, $hfh);
}

sub get {
    my ($file, $use_url) = @_;
    my $path_info;
    # use path if stored in cached header/complete file
    if($use_url && (my $url = get_original_url($file))) {
	$path_info= $url;
    }
    else {
	$path_info=$file;
	$path_info=~s/^/\//;
	$path_info=~s/_/\//g;
    }
    defined(my $refresh_pid = open(my $fh, '|-')) || die "Failed to open refresh pipe: $!";
    if ($refresh_pid){
	printmsg "Get $path_info\n";
	print $fh "$_\r\n" foreach ("HEAD $path_info", 'Cache-Control: max-age=0', 'Connection: Close', '');
	close($fh);	
    }
    else {
	close (STDOUT);
	open (STDOUT, '>', '/dev/null') || die $!;
	local $ENV{REMOTE_ADDR} = 'CLEANUPREFRESH';
	local @ARGV = ('-i', '-c', $configfile);
	do '/usr/share/apt-cacher/apt-cacher';
    }

    waitpid($refresh_pid,0);
    if(($? > 0) && ! $force) {
	die "Unable to update $path_info (status: $?).\nCleanup aborted to prevent deletion of cached data.\n";
    }
    return $?;
}

sub pdiff {
    my ($name) = @_;
    if (!-f $name) {
	warn ("File $name not found\n");
	return;
    }

    if ($name !~ /main|contrib|non-free/) {
	printmsg "Upstream repository for $name not standard hierarchy, skipping attempting to patch\n";
	return;
    }
    my ($basename,$type) = ($name =~ /(^.+?)(\.(?:bz2|gz))?$/);
    (my $release = $basename) =~ s/(?:main|contrib|non-free).*$/{In,}Release/;
    (my $diffindex = $basename) .= '.diff_Index';

    my ($release_fh, $diffin_fh);

    foreach my $glob_fh ([\$release, \$release_fh], [\$diffindex, \$diffin_fh]) {
	foreach (glob(${$glob_fh->[0]})) {
	    get($_) unless $offline;
	    open(${$glob_fh->[1]}, '<', $_)
	      && do {
		  ${$glob_fh->[0]} = $_;
		  last;
	      }
		|| printmsg("Failed to open $_: $!\n");
	}
	return unless ${$glob_fh->[1]}->opened;
	_flock(${$glob_fh->[1]}, LOCK_SH) || die("Cannot lock ${$glob_fh->[0]}: $!");
    }

    # Read Release file
    (my $diffindex_patt = $diffindex) =~ s/^.*(main|contrib|non-free.*)/$1/;
    (my $name_patt = $name) =~ s/^.*(main|contrib|non-free.*)/$1/;
    for ($diffindex_patt, $name_patt) {
	s/_/\//g;
    }
#    printmsg "Searching $release for $diffindex_patt and $name_patt\n";

    my ($diffindex_sha1, $name_sha1, $name_size);
    while (<$release_fh>) {
	if (/^\s(\w{40})\s+\d+\s$diffindex_patt\n/) {
	    $diffindex_sha1 = $1;
#	    printmsg "Found! $diffindex_patt $1\n";
	}
	elsif (/^\s(\w{40})\s+(\d+)\s$name_patt\n/) {
	    $name_sha1 = $1;
	    $name_size = $2;
#	    printmsg "Found! $name_patt $1 $2\n";
	}
	last if ($name_sha1 && $diffindex_sha1);
    }
    _flock($release_fh, LOCK_UN);
    close($release_fh);
    if (!$name_sha1 || !$name_size || !$diffindex_sha1) {
        warn "SHA1s for $name_patt and/or $diffindex_patt not found in $release, aborting patch\n";
	return;
    }

    my $sha1 = Digest::SHA->new(1); # SHA1
    my $digest;

    (my ($cfh, $hfh) = open_lock($name)) == 2 || do {
	warn "Failed to open filehandles for $name, aborting patch\n";
	return;
    };

    # Check size first
    if (-s $cfh == $name_size) {
	printmsg ("$name matches size in $release, going on to check SHA1..\n");
	
	# Check SHA1 only if size correct
	$digest = $sha1->addfile($cfh)->hexdigest;
	if ($digest eq $name_sha1) {
	    printmsg "$name already matches SHA1 in $release: patching not required\n";
	    return 1 # success
	}
	else {
	    printmsg "$name SHA1 not latest: proceeding with patch\n";
	}
    }
    else {
	printmsg ("$name size not latest, proceeding with patch\n");
    }

    my $raw = IO::Uncompress::AnyUncompress->new($name)
      or die "Decompression failed: $AnyUncompressError\n";

    open (my $tfh, "+>", undef)|| die "Unable to open temp file: $!";
	
    printmsg "Reading $basename...\n";
    while (<$raw>){
	last if $AnyUncompressError;
	print $tfh $_;
	$sha1->add($_);
    }
    close($raw);

    if ($AnyUncompressError) {
	warn "$name Read failed: $AnyUncompressError. Aborting patch\n";
	return;
    }
	
    $digest = $sha1->hexdigest;
    # printmsg "$basename SHA1: $digest\n";

    # Read diff_Index
    my (@hist, @patch);

    my $diffindex_digest = $sha1->addfile($diffin_fh)->hexdigest;
    if ($diffindex_digest ne $diffindex_sha1) {
	_flock($diffin_fh, LOCK_UN);
	close ($diffin_fh);
	if ($force) {
	    warn "$diffindex incorrect SHA1: expected $diffindex_sha1, got $diffindex_digest. Continuing anyway as --force specified\n";
	}
	else {
	    warn "$diffindex incorrect SHA1: expected $diffindex_sha1, got $diffindex_digest. Aborting patch. Use --force to ignore\n";
	return;
	}
    }
    seek($diffin_fh,0,0) || die "Seek failed: $!"; # rewind
    my $curr= <$diffin_fh>; # read first line
    chomp $curr; # remove trailing \n
#    printmsg "$diffindex: $curr\n";
    my ($target_sha1, $target_size) = (split (/\s+/,$curr))[1,2];
    if ($digest eq $target_sha1) { # check this matches /SHA1/
	printmsg "SHA1 match: $name already up to date\n";
	_flock($diffin_fh, LOCK_UN);
	close ($diffin_fh);
	return 1; # success
    }
    else {
	while (<$diffin_fh>) {
	    next if (/^SHA1-History:/); # skip header
	    last if (/^SHA1-Patches:/);# end of history
	    push @hist, $_;
	    next;
	}
	while (<$diffin_fh>) {
	    push @patch, $_; # To EOF
	    next;
	}
    }
    _flock($diffin_fh, LOCK_UN);
    close ($diffin_fh);

    my $diff;
    my $count=0;
    for (@hist) {
	my @line;
	@line = split;
#	printmsg "Checking $digest against @line\n";
	if ($digest eq $line[0]) {
#	    printmsg "found SHA1 match at \$hist $count: $line[0]\n";
	    $diff = $count;
	    last
	}
	$count++;
    }
    if (!defined $diff) {
	warn "Existing SHA1 not found in diff_Index, aborting patch\n";
	return;
    }

    my $diffs=''; # Initialise to work around perl bug giving "Use of uninitialized value error"
	
    open(my $diffs_fh, ">", \$diffs) || die "Failed to open in memory diff file: $!";
    for (@patch[$diff .. $#patch]) {
	my ($pdiffsha1, $size, $suff) = split;
	my $pdiff = "$basename.diff_$suff.gz";
	if (!-f $pdiff) {
	    if (!$offline) {
		get($pdiff);
	    }
	    if (!-f $pdiff) {
		warn("$pdiff not available, aborting patch");
		return;
	    }
	}
	printmsg "Reading $pdiff\n";
	my $pdfh = IO::Uncompress::AnyUncompress->new($pdiff)
	  or die "Decompression failed: $AnyUncompressError\n";
	while (<$pdfh>) {
	    last if $AnyUncompressError;
	    print $diffs_fh $_;
	    $sha1->add($_);
	}
	close($pdfh);
	if ($AnyUncompressError) {
	    warn "$name Read failed: $AnyUncompressError. Aborting patch\n";
	    return;
	}

	my $pdiffdigest = $sha1->hexdigest;
#	printmsg "$pdiff SHA1: $pdiffdigest\n";
	if ($pdiffsha1 ne $pdiffdigest) {
	    warn "$pdiff SHA1 incorrect: got $pdiffdigest, expected $pdiffsha1, aborting patch";
	    return;
	}
    }
    close($diffs_fh);

    fcntl($tfh, F_SETFD, 0)
      or die "Can't clear close-on-exec flag on temp filehandle: $!\n";
    my $cwd = Cwd::cwd(); # Save
    chdir '/dev/fd' or  die "Unable to change working directory: $!";
    open(my $patchpipe, '|-', "$patchprog ".fileno($tfh)) ||  die "Unable to open pipe for patch: $!";
    printmsg "Patching $name with $patchprog\n";
    print $patchpipe $diffs;
    print $patchpipe "w\n"; # ed write command
    close($patchpipe);
    chdir $cwd or die "Unable to restore working directory: $!"; # Restore
    my $rstat =($? >> 8);
    if ($rstat) {
	warn "Patching failed (exit code $rstat), aborting\n";
	return;
    }
    printmsg "Verifying patched file\n";
    if (-s $tfh != $target_size) {
	warn "$name patching failed! $tfh is not size $target_size\n";
	return;
    }
    seek($tfh,0,0) || die "Seek failed: $!"; # rewind
    $sha1->addfile($tfh);
    $digest=$sha1->hexdigest;
    if ($digest eq $target_sha1) {
	printmsg "Success! SHA1: $digest\n";
	if ($sim_mode) {
	    printmsg "Simulation mode, so not replacing existing files\n";
	}
	else {
	    printmsg "Saving as $name\n";
	    seek($tfh, 0, 0) || die "Seek failed: $!"; # rewind
	    truncate($cfh, 0) || die "Truncate failed: $!";
	    seek($cfh, 0, 0) || die "Seek failed: $!";
	    my ($z,$encoding) = ($name=~/bz2$/ ?
				 ((IO::Compress::Bzip2->new($cfh)), "x-bzip2") :
				 ($name=~/gz$/ ?
				  ((IO::Compress::Gzip->new($cfh, -Level => 9)), "x-gzip") :
				  $cfh));

	    while (<$tfh>) {
		$z->print($_);
	    }
	    close($z);
	    $cfh->flush; # So the size is correct
	    my $datestring = HTTP::Date::time2str;
	    my $response = HTTP::Response->new(200, 'OK', ['Date' => $datestring,
							   'Content-Length' => -s $cfh,
							   'Content-Type' => 'text/plain',
							   'Last-Modified' => $datestring]);
	    $response->header('Content-Encoding' => $encoding) if $encoding;;
	    write_header($hfh, $response);
	    _flock($cfh, LOCK_SH); # Downgrade
	    _flock($hfh, LOCK_UN);
	    # Read checksums
	    if ($cfg->{checksum}) {
		printmsg ("Importing new checksums from patched $name\n");
		import_sums($name, $cfh);
	    }
	}
    }
    else {
	warn "$name patching failed! Patched SHA1 is $digest, expecting $target_sha1\n";
	return;
    }
    close $tfh;
    return 1; # success
}

# Calls _db_compact to do the work and reports results
# Arg: DB handle ref
sub db_compact {
    my ($dbh) = @_;
    printmsg "Compacting checksum database....\n";
    while (my ($status, %results) = @{_db_compact($dbh)}) {
	if ($status) {
	    printmsg "db_compact failed: $status\n";
	    last;
	}
	else {
	    printmsg " Compacted ". $results{compact_pages_free} ." pages\n Freed ". $results{compact_pages_truncated} ." pages\n";
	    if ($results{compact_pages_free} + $results{compact_pages_truncated} == 0) {
		printmsg "Done!\n";
		last;
	    }
	}
    }
    return;
}

#############################################################################
# Manipulate checksum database
if (@db_mode || $db_recover){

    my $ok_chars = '-a-zA-Z0-9+_.,~^$*?{}[]()'; # Acceptable characters for user input
    $ok_chars .= '/' if $cfg->{distinct_namespaces};
    print "Checksum database mode\n";

    if (!$cfg->{checksum}) {
	die "$0: checksumming not enabled. Use --force to override\n" if !$force;
	print "$0: checksumming not enabled, but forced to continue\n";
    }
    $verbose = 1; # Just for now

    if ($db_recover) {
	printmsg "Running database recovery...";
	db_recover();
	printmsg "Done!\n";
    }

    chdir "$cfg->{cache_dir}/packages" || die "Unable to enter cache package dir: $!";

  SWITCH:
    while (local $_ = shift @db_mode) {
	/^import$/ && do {
	    foreach (glob('*es.bz2 *es.gz *es *Release *diff_Index')) {
		open(my $fh, '<', $_)|| do {
		    warn "Failed to open $_ for import: $!";
		    next;
		};
		printmsg "Importing checksums from $_\n";
		import_sums($_, $fh) if !$sim_mode;
	    }
	    next SWITCH;
	};
	/^compact$/ && do {
	    db_compact(db());
	    next SWITCH;
	};
	/^(?:dump|search)$/ && do {
	    my $re;
	    if (/^search$/){
		$re = shift @db_mode;
		die "No search expression given\n" if !$re;
		die "Invalid character '$1' in search\n" if $re =~ /([^$ok_chars])/o; # sanitize
	    }
	    my $cursor = get_cursor(db());
	    my ($filename,$data) = ('','');
	    while (cursor_next($cursor, \$filename, \$data) == 0)
	      {
		  next if /^search/ && $filename !~ /$re/;
		  print "$filename\n";
		  my $href = hashify(\$data);
		  while (my ($k,$v) = each %$href) {
		      $v = '' if ! defined $v;
		      print " $k: $v\n";
		  }
	      }
	    next SWITCH;
	};
	/^delete$/ && do {
	    my $re = shift @db_mode;
	    die "No give regex to match files to delete\n" if !$re;
	    die "Invalid character '$1' in pattern\n" if $re =~ /([^$ok_chars])/o; # sanitize
	    my $cursor = get_cursor(db(),1);
	    my ($filename,$data) = ('','');
	    while (cursor_next($cursor, \$filename, \$data) == 0)
	      {
		next if $filename !~ /$re/;
		printmsg "Deleting data for $filename\n";
		$cursor->c_del == 0 || warn "c_del failed: $BerkeleyDB::Error" if !$sim_mode;
	    }
	    next SWITCH;
	};
	/^failcheck$/ && do {
	    printmsg 'Connecting to database....';
	    # Just connect to the database which runs failchk()
	    if (db(1)) { # Without locking
		printmsg "Success!\n";
	    }
	    next SWITCH;
	};
	/^verify$/ && do {
	    printmsg "Waiting for exclusive lock...";
	    if (db_flock(LOCK_EX)){
		printmsg "Got it!\nVerifying database...";
		printmsg db_verify("$cfg->{cache_dir}/sums.db", temp_env()) ? "Failed! " . db_error() . "\n" : "Passed!\n";
	    }
	    else {
		warn "Unable to get exclusive database lock: $!\n";
	    }
	    next SWITCH;
	};
	warn "Unknown command $_ \n";
	next SWITCH;
    }

    exit;
}

#############################################################################

# Cache cleaning from here

# Take a lock on the cache dir to ensure only one cleanup script can run at a
# time as this can take a while on some systems.

open  (my $cleanup_lock, '<', $cfg->{'cache_dir'})
  or die "Can't open $cfg->{'cache_dir'} for locking!\nError: $!\n";
flock($cleanup_lock, LOCK_EX|LOCK_NB)
  or die "Another apt-cacher-cleanup is already running. Exiting!\n";

# check offline mode in config
if (defined $cfg->{offline_mode} && $cfg->{offline_mode}) {
	$offline = 1;
}

use GDBM_File;
open(my $tmpfile, "+>", undef) or die $!;
tie my %valid, 'GDBM_File', fd_path($tmpfile), &GDBM_NEWDB|&GDBM_FAST, oct(600) # Does a separate open
  or die "GDBM_File tie failed: $!";
close($tmpfile); # So we can close this

### Preparation of the package lists ########################################

chdir "$cfg->{cache_dir}/packages" && -w "." || die "Could not enter the cache dir: $!";

if($> == 0 && !$cfg->{user} && !$force) {
    die "Running $0 as root\nand no effective user has been specified. Aborting.\nPlease set the effective user in $configfile or use --force to ignore\n";
}

# Try to ensure corresponding Packages/Sources is present for each diff_Index
# and Release for each Packages/Sources
{
    my %missing;
  CHECKFILE:
    foreach (glob('*diff_Index *{Packages,Sources}{,.gz,.bz2}')) {
	my $file = $_;
	if (s/\.diff_Index$/{,.gz,.bz2}/) {
	    printmsg "Checking for $_ for $file\n";
	}
	elsif (s/(?:dists_[^_]+_(?:updates_)?\K(?:[^_]+_){2})?(?:Packages|Sources)(?:\.(?:bz2|gz))?$/{In,}Release/) {
	    printmsg "checking for $_ for $file\n";
	}
	foreach (glob) {
	    if ( -f $_ ) {
		printmsg "Found $_\n";
		next CHECKFILE;
	    }
	}
	$missing{(glob)[-1]} = 1;
    }
    get($_) foreach keys %missing;
}


# Initially preserve the index files
%valid = map {$_ => 1} glob('*{Release,diff_Index} *{Packages,Sources}{,.gz,.bz2}');

foreach my $file (keys %valid) {

    # Try to patch
    my $patched;
    if($pdiff_mode && $file =~ /(?:Packages|Sources)(?:\.(?:bz2|gz))?$/) {
	printmsg "Attempting to update $file by patching\n";
	($patched = pdiff($file)) || printmsg "Patching failed or not possible\n";
    }
    # If patching failed download them, unless offline
    if (!$patched) {
	if(!$offline) {
	    get($file);
	}
	else {
	    printmsg "Offline: Reusing existing $file\n";
	}
    }

    # Remove obsolete Release and Release.gpg
    if ((my $obsolete = $file) =~ s/InRelease/Release/) {
	next unless exists $valid{$obsolete};
	printmsg "Removing $obsolete in favour of $file\n";
	delete $valid{$obsolete};
	unlink $obsolete, "$obsolete.gpg" if !$sim_mode;
    }

}

foreach my $file (keys %valid) {

    printmsg "Reading: $file\n";

    (my ($cfh, $hfh) = open_lock($file)) == 2 || do {
	die "Failed to open filehandles for $file. Resolve this manually. \nExiting to prevent deletion of cache contents.\n";
    };

    extract_sums($file, $cfh, \%valid) || die("Error processing $file in $cfg->{cache_dir}/packages, cleanup stopped.\nRemove the file if the repository is no longer interesting and the packages pulled from it are to be removed.\n");
}

printmsg "Found ".scalar (keys %valid)." valid file entries\n";
#print join("\n",keys %valid);

# Build a source package version reverse hash for changelog validation from the .dsc files
printmsg "Building source package file/version table\n";
my %svrhash = map {m#([-+.a-z0-9]+_(?:\d:)?[-+.~a-zA-Z0-9]+)\.dsc# && $1 => 1} keys %valid;

# Remove old checksum data
if ($cfg->{checksum}) {

    my $dbh = db();

    my $do_compact;
    $dbh && do {
	printmsg "Removing expired entries from checksum database\n";
	
	my $cursor = get_cursor($dbh,1);
	my ($filename,$data)=('','');
	while (cursor_next($cursor, \$filename, \$data) == 0)
	  {
	      next if defined $valid{$filename};
	      printmsg "Deleting checksum data for $filename\n";
	      $cursor->c_del == 0 || warn "c_del failed: $BerkeleyDB::Error" if !$sim_mode;
	      $do_compact = 1;
	  }
	db_compact($dbh) if $do_compact || $pdiff_mode;
    };
}

# Clean package directory
foreach (glob('*{,/*}')) {
    next if -d; # Skip directories
    if (/([-+.a-z0-9]+_(?:\d:)?[-+.~a-zA-Z0-9]+)_changelog$/ && !$svrhash{$1}) {
	unlink $_, "../headers/$_" unless $sim_mode;
	printmsg "Removing expired changelog: $_ and company...\n";
	next;
    }

    next unless is_file_type('package', $_) || is_file_type('pdiff', get_original_url($_)); # Package and pdiff files only

    if(! defined($valid{$_})) {
	unlink $_, "../headers/$_", "../private/$_.complete" unless $sim_mode;
	printmsg "Removing file: $_ and company...\n";
    }
    else {
	# Verify SHA1 checksum
	my $target_sum = hashify(\$valid{$_})->{sha1};
	next unless $target_sum;
	# print "Validating SHA1 $target_sum for $_\n";
	open(my $fh, '<', $_) || die "Unable to open file $_ to verify checksum: $!";
	flock($fh, LOCK_EX);
	if (is_file_type('pdiff', get_original_url($_))) { # pdiffs need decompressing
	    $fh = IO::Uncompress::AnyUncompress->new($fh)
	      or die "Decompression failed: $AnyUncompressError\n";
	}
	if ((my $sha1 = Digest::SHA->new(1)->addfile($fh)->hexdigest) ne $target_sum) {
	    unlink $_, "../headers/$_", "../private/$_.complete" unless $sim_mode;
	    printmsg "Checksum mismatch ($target_sum <=> $sha1): $_, removing\n";
	}
	# No explicit LOCK_UN: it fails with IO::Uncompress::AnyUncompress, just rely on close
	close $fh;
    }
}

# Clean header directory
chdir "$cfg->{cache_dir}/headers" && -w "." || die "Could not enter the cache header dir: $!";

foreach (glob('*{,/*}')) {
    next if -d; # Skip directories
    if((is_file_type('package', $_) && !defined($valid{$_})) # Not indexed
       || !-e "../packages/$_") { # No corresponding package
	unlink $_, "../private/$_.complete" unless $sim_mode;
	printmsg "Removing expired headers: $_ and company...\n";
	next;
    }
    my $resp = read_header($_);
    next if $resp && $resp->is_success; # Don't cache errors any more
    printmsg "Removing cached error/invalid response: $_\n";
    delete $valid{$_};
    unlink $_ unless $sim_mode;
}

# Remove .complete files, for which we no longer have cached data.  No new
# complete files are being created, but they do contain the URL that was used,
# so keep those for now
chdir "$cfg->{cache_dir}/private" && -w "." || die "Could not enter the cache private dir: $!";
foreach (glob('*.complete')) {
   s/.complete$//;
   if(!(defined($valid{$_}) && -e "../packages/$_" && -e "../headers/$_")) {
      printmsg "Removing: $_.complete\n";
      unlink "$_.complete" unless $sim_mode;
   }
   elsif (my $resp = read_header("../headers/$_")) {
       next unless $resp->header('X-AptCacher-URL');
       # Remove complete files if we now have the upstream URL in the headers
       printmsg "Removing redundant $_.complete\n";
       unlink "$_.complete" unless $sim_mode;
   }
}

# last step, kill some zombies
foreach (glob('*.notify')) {
    printmsg "Removing obsolete notify file: $_\n";
    unlink $_ unless $sim_mode;
}

chdir "$cfg->{cache_dir}/packages" || die "Could not enter the cache package dir: $!";

foreach (glob('*{,/*}')) {
    # must be empty and not complete and being downloaded right now
    if(-z $_) {
	my $fromfile;
	if(open($fromfile, '<', $_) && flock($fromfile, LOCK_SH|LOCK_NB)) {
	    # double-check, may have changed while locking
	    if(-z $fromfile) {
		printmsg "Removing zombie files: $_ and company...\n";
		unlink $_, "../headers/$_", "../private/$_.complete" unless $sim_mode;
		_flock($fromfile, LOCK_UN);
		close($fromfile);
	    }
	}
    }
}
