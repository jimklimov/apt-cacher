#!/usr/bin/perl

# install.pl
#
# Setup script for apt-cacher
#
# Safe to run multiple times; later versions of this script will remove obsolete
# directories or files and not touch required directories or files.

use strict;
use warnings;

use lib '/usr/share/apt-cacher/lib';

umask 0022;

#############################################################################
### configuration ###########################################################
# Include the library for the config file parser
require('apt-cacher.pl');

# Read in the config file and set the necessary variables
our $cfg = eval{ read_config('/etc/apt-cacher/apt-cacher.conf') };

# not sure what to do if we can't read the config file...
die "Could not read config file: $@" if $@;

################################################
# Check that the cache_dir has been set and continue on (note: this should never happen
# because cache_dir is preset to a default value prior to loading the config file)
die "Warning: config file could not be parsed (cache_dir is not set)\n" if ($cfg->{cache_dir} eq '');

my @permcmd;
if ($cfg->{user} && $cfg->{group} ) {
    @permcmd = ('chown', "$cfg->{user}:$cfg->{group}");
}
elsif(-e $cfg->{cache_dir}) {
    @permcmd = ("chown", "--reference", $cfg->{cache_dir});
}
elsif(my @info=getpwnam("www-data")) {
    print "Assuming www-data is the user ID used to run apt-cacher\n";
    @permcmd = ("chown", "$info[2]:$info[3]");
}
else {
    @permcmd = ("/bin/echo", "User account for apt-cacher/http daemon unknown, please set ownership for the following files manually:");
}

# Create top-level directories
foreach my $dir ($cfg->{cache_dir}, $cfg->{log_dir}) {
    if (!-d $dir) {
	print "Doing mkdir($dir, 0755)\n";
	mkdir($dir, 0755);
	system (@permcmd, $dir);
    }
    if (!-w $dir) {
	die "Warning, $dir exists but is not is not writable for apt-cacher!\n";
    }
}

# Check rest of installation
check_install();

# Remove these files/directories if they exist (obsolete)
foreach my $rmdir ("$cfg->{cache_dir}/tmp", "$cfg->{cache_dir}/temp", "$cfg->{cache_dir}/head") {
    if (-d $rmdir) {
	print "Doing 'rm -rf $rmdir' (obsolete)\n";
	system("rm -rf $rmdir");
    }
}

for ("README", "README.txt") {
   my $file=$cfg->{cache_dir}."/$_";
   if (-f $file) {
      print "Found obsolete file $file - removing.\n";
      unlink($file);
   }
}

# Run database recovery and reset semaphores
if ($cfg->{checksum}) {
    require('apt-cacher-cs.pl');
    setup_ownership();
    print "Clearing database locks...";
    db(1); # Connect without environment lock which runs failchk()
    print "Done!\n";
    if (my $sem = get_existing_sem()) { # Existing only
	print "Clearing semaphore block\n";
	$sem->remove;
    }
}

exit(0);
