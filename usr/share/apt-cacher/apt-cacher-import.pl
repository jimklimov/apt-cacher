#!/usr/bin/perl

# apt-cacher-import.pl
#
# Script to import APT package files for into the apt-cacher cache.
#
# It is not necessary to run this script when setting up apt-cacher for the
# first time: its purpose is to introduce APT packages from some other source,
# such as a local mirror. Along with each cached file, apt-cacher also caches
# the HTTP headers to send out to clients when the package is requested. If
# package files are just copied straight into the cache, apt-cacher won't use
# them because the relevant headers are missing. Having copied some package
# files to the import directory, or by specifying the directory to use on the
# command line, utilise this script to generate the HTTP headers and copy the
# header and package files to the correct location.
#
# Basic usage:
# 1. Place package files into {cache_dir}/import
# 2. Run the script: /usr/share/apt-cacher/apt-cacher-import.pl
#
# It can also be used in sevaral ways to import packages within namespaces when
# distinct_namespaces is set in the apt-cacher configuration.
#
# To place imported package in a specified namespace:
# 1. As basic usage above, but also use -n|--namespace foobar
#
# To migrate an existing apt-cacher cache to support multiple distributions:
# 1. Configure distinct_namespaces = 1 in the configuration file
# 2. Run the script with the -u|--url option and specify the cache package
#    directory as the import directory, e.g
#
#     /usr/share/apt-cacher/apt-cacher-import.pl -u /var/cache/apt-cacher/packages
#
# To import only packages which are in the current cache index files:
# 1. Configure apt-cacher with distinct_namespaces = 1 in the configuration file
# 2. Run apt-get update on your clients to populate the apt-cacher index files
# 3. Run the script with the -d|--digest option and optionally specify a package
#    import directory, e.g
#
#     /usr/share/apt-cacher/apt-cacher-import.pl -d [/tmp/obsolete_cache]
#
# Copyright (C) 2004, Jonathan Oxer <jon@debian.org>
# Copyright (C) 2005, Eduard Bloch <blade@debian.org>
# Copyright (C) 2011, Mark Hindley <mark@hindley.org.uk>
#
# Distributed under the terms of the GNU Public Licence (GPL).

use strict;
use warnings;

# Include the library file
use lib '/usr/share/apt-cacher/lib';
require('apt-cacher.pl');

use Getopt::Long qw(:config no_ignore_case bundling);
use File::Copy;
use File::Spec;
use Cwd 'abs_path';
use HTTP::Date;
use HTTP::Response;
use URI;
use Digest::MD5;

my $configfile = '/etc/apt-cacher/apt-cacher.conf';
my $help;
my $quiet;
my $sim;
my $force;
my @namespaces;
my $digest;
my $url;
my $recursive;
my $ro_mode;
my $symlink_mode;

local $| = 1;

my %options = (
	       "h|help" => \$help,
	       "q|quiet" => \$quiet,
	       "s|show" => \$sim,
	       "f|force" => \$force,
	       "n|namespace=s" => \@namespaces,
	       "d|digest" => \$digest,
	       "u|url" => \$url,
	       "R|recursive" => \$recursive,
	       "r|readonly" => \$ro_mode,
	       "l|symlinks" => \$symlink_mode,
	       "c|cfg|conf=s" => \$configfile
	      );

if (!GetOptions(%options) || $help) {

    die <<EOM
Usage: $0 [-c|--cfg|--conf=<configfile>]
 [-d|--digest] [-f|--force] [-h|--help] [-l|--symlinks]
 [-n|--namespace=<namespace>] [-q|--quiet] [-R|--recursive] [-r|--readonly]
 [-s|--show] [-u|--url] [<import_dir> ...]

Options:
 -c <configfile> Use the specified configuration file
		 (default '$configfile').
 -d		 Automatically try to determine the correct namespace for the
		 packages by matching MD5 digests with those in Packages and
		 Sources indices (requires distinct_namespaces).

 -f		 Force continue, even if it seems unwise.
 -h		 Show this usage help.
 -l		 Do not move the source files. Instead, create symlinks to them.
		 If the target symlink already exists, it will be removed.
 -n <namespace>  Use the specified namespace subdirectory (requires
                 distinct_namespaces).
 -q		 Less verbose.
 -R		 Recurse into subdirectories below the import directory.
 -r		 Do not move the source files. Instead, create hardlinks or real
		 copies.
 -s		 Just show what would be done.
 -u		 Automatically try to determine the correct namespace for the
		 packages from their original URL (requires distinct_namespaces).

 If <import_dir> is omitted, {cache_dir}/import will be used.
EOM
}

our $cfg = eval{ read_config($configfile) };

# not sure what to do if we can't read the config file...
die "Could not read configuration file '$configfile': $@" if $@;

private_config();

# change uid and gid
setup_ownership($cfg);

check_install();

# Sanity checks
if(!$ARGV[0]) {
    my $import_dir = "$cfg->{cache_dir}/import";
    print "No import directory specified, using $import_dir\n" unless $quiet;
    push @ARGV, $import_dir;
    sleep 2;
}

if (@namespaces || $digest || $url) {
    if (!$cfg->{distinct_namespaces}) {
	die "Namespace import requested without distinct_namespaces being set in the configuration.\n";
    }
    elsif ((grep {defined && $_ > 0} (scalar @namespaces, $url, $digest)) > 1) {
	die "Cannot specify multiple simultaneous namespace mechanisms\n";
    }
    elsif ($url && $recursive) {
	die "Cannot act recursively with -u|--url\n";
    }
    elsif (@namespaces){
	foreach (@namespaces) {
	    if (!exists $cfg->{_path_map}{$_}) {
		print "The namespace specified ($_) is not a path_map key. This is almost certainly not what was intended.\n";
		die "Use -f|--force option to import anyway.\n" unless $force;
		print "Continuing anyway as -f|--force specified.\n";
	    }
	}
    }
    elsif ($digest && ! ($ro_mode || $symlink_mode)) {
	warn "Digest mode requires symlinking or copying: enabling symlink option\n";
	$symlink_mode = 1;
    }
}

die "Cannot specify -s|--symlink and -r|--readonly together.\n" if $symlink_mode && $ro_mode;

my $imported = 0;

# common dummy data for all imported packages
my @info = stat("$cfg->{cache_dir}/private");
my $headerdate = time2str();

my %digest_map; # To map MD5 digests to namespaces for -d|--digest
read_indices();

foreach (@ARGV) {
    if (!-d) {
	warn "$_ is not a directory -- skipping\n";
	next;
    }
    print "Importing from $_\n" unless $quiet;
    import($_);
}

print "Done.\n" ,
  ($sim ?
   "Simulation mode so nothing actually done. $imported files would have been imported\n" :
   "$imported files imported\n") unless $quiet;

exit 0;

### Subroutines ###

sub read_indices {

    if ($digest) {
	my $cwd=Cwd::getcwd();
	chdir("$cfg->{cache_dir}/packages") || die "Unable to chdir() to $cfg->{cache_dir}/packages/: $!";
	# Read the index files
	my @ifiles = glob("*{Packages,Sources}{,.bz2,.gz}");
	die "No index files found in $cfg->{cache_dir}/packages for automatic digest import into namespaces\n" unless @ifiles;
	print "Reading index files from $cfg->{cache_dir}/packages/\n", "This can take some time, so be patient....\n" unless $quiet;
	foreach my $indexfile (@ifiles) {
	    my %sums;

	    # Get namespace
	    my $namespace;
	    if (my $uri = get_original_url($indexfile)) {
		unless ($namespace = get_namespace($uri)) {
		    die "No namespace found for $indexfile. Check path_map setting or delete the file. Aborting\n";
		}
	    }
	    else {
		warn "Unable to get original URL for $indexfile\n";
		next;
	    }

	    print "Reading: $indexfile [namespace $namespace]\n" unless $quiet;

	    # Parse
	    extract_sums($indexfile, undef, \%sums)
	      || die("Error processing $indexfile, automatic digest import abandoned.\nRemove the file if the packages to be imported are not associated with this repository.\n");

	    while (my ($package, $ice) = each %sums) {
		if (my $md5 = hashify(\$ice)->{md5}) {
		    # Use a reverse hash to prevent duplicates
		    my %h = map {$_ => 1} @{$digest_map{$md5}{namespaces}};
		    next if $h{$namespace};
		    push @{$digest_map{$md5}{namespaces}}, $namespace;
		    $digest_map{$md5}{filename} = (File::Spec->splitpath($package))[2];
		}
		else {
		    warn "MD5 digest for $package not included in indexfile.\n";
		}
	    }
	}
	chdir $cwd;
    }
    return;
}

sub import {
    my $import_dir=shift;
    chdir($import_dir) || die "Can't change to the import directory ($import_dir)";

    if($recursive) {
	my $cwd=Cwd::getcwd();
	foreach (glob('*')) {
	    if(-d && !-l) {
		import("$cwd/$_");
		chdir $cwd;
	    }
	}
    }

    print "Simulation mode: just showing what would be done\n" if $sim;
    print(($symlink_mode ? 'Symlinking' :
	   ($ro_mode ? 'Hard linking or copying' :
	    'Moving')), " package files from $import_dir to $cfg->{cache_dir}\n") unless $quiet;

    # Loop through all the package files in the current directory
    foreach my $packagefile (glob('*')) {
	next unless -f $packagefile;
	next unless $digest || is_file_type('package', $packagefile);

	# By default use the current name
	my $targetfile = $packagefile;

	if($digest) {
	    open(my $pfh, '<', $packagefile) || die "Unable to open $packagefile: $!";
	    my $md5 = Digest::MD5->new()->addfile($pfh)->hexdigest;
	    close($pfh);
	    if (! exists $digest_map{$md5}) {
		print "Cannot import $packagefile: file MD5 digest $md5 not found in index files\n";
		next;
	    }
	    @namespaces = @{$digest_map{$md5}{namespaces}};
	    $targetfile = $digest_map{$md5}{filename}; # Just in case filename is mangled
	}
	elsif ($url) {
	    if (my $uri = get_original_url($packagefile)) {
		if (my $namespace = get_namespace($uri)) {
		    push @namespaces, $namespace;
		}
		else {
		    print "Unable to determine namespace from URL for $packagefile. Skipping.\n";
		    next
		}
	    }
	    else {
		die "Unable to get original URL for $packagefile\n";
	    }
	}

	@namespaces = (undef) unless @namespaces;
      NAMESPACE:
	foreach (@namespaces) {

	    my $package_dir = join('/', grep {defined} $cfg->{cache_dir}, 'packages', $_);
	    my $header_dir = join('/', grep {defined} $cfg->{cache_dir}, 'headers', $_);

	    foreach ($package_dir, $header_dir) {
		next if $sim;
		mkdir $_ or my $error = $! unless -d;
		die "Failed to create directory $_: $error\n" unless -d;
		die "Cannot write to $_: $!\n" if !-w;
	    }

	    foreach ("$header_dir/$targetfile", "$package_dir/$targetfile"){
		if (-f) { # Don't overwrite files
		    print "$_ already exists: skipping\n";
		    next NAMESPACE;
		}
	    }

	    print "Importing: $packagefile",
	      ($packagefile ne $targetfile ? " as $targetfile" : ''),
		($_ ? " [namespace $_]\n" : "\n") unless $quiet;

	    unless ($sim) {
		import_file($packagefile, "$package_dir/$targetfile");

		if ($url && -f "../headers/$packagefile") {
		    # Upgrading an existing cache, use the header file
		    import_file("../headers/$packagefile", "$header_dir/$packagefile"); #
		}
		else {
		    # Generate a header
		    write_header("$header_dir/$targetfile",
				 HTTP::Response->new(200, 'OK', ['Date' => $headerdate,
								 'Last-Modified' => $headerdate,
								 'Content-Length' => -s "$package_dir/$targetfile"]));
		}

		# copy the ownership of the private directory
		foreach (glob("{$header_dir,$package_dir}/$targetfile")) {
		    chown($info[4], $info[5], $_) ||
		      warn "Failed to set ownership for $_. You need to correct this manually\n";
		}
	    }
	    $imported++;
	}
    }
    return;
}

sub import_file {
    my ($source, $target) = @_;

    if($symlink_mode) {
	symlink(abs_path($source), $target) ||
	  (unlink($target) && symlink(abs_path($source), $target)) ||
	    die "Failed to create the symlink $target";
    }
    elsif($ro_mode) {
	link($source, $target) || copy($source, $target) || die "Failed to copy $source";
    }
    else {
	rename($source, $target) || die "Failed to move $source to $target: $!.\n Try read-only (-r) or symlink (-l) options.";
    }
    return 1;

}
