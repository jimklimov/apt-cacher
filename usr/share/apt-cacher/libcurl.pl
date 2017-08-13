#!/usr/bin/perl

# libcurl.pl
#
# Script to pass command to libcurl process.
# Only used to get it to exit at the moment.
#
# Copyright (C) 2011, Mark Hindley <mark@hindley.org.uk>

use strict;
use warnings;
use lib '/usr/share/apt-cacher/lib';

require('apt-cacher.pl');

# Read in the config file and set the necessary variables
our $cfg = eval{ read_config('/etc/apt-cacher/apt-cacher.conf') };

die "Could not read config file: $@" if $@;

use IO::Socket::UNIX;
$cfg->{libcurl_socket} = "$cfg->{cache_dir}/libcurl.socket" unless $cfg->{libcurl_socket};
if (my $conn = IO::Socket::UNIX->new($cfg->{libcurl_socket})) {
    print $conn $_, "\n\n" foreach @ARGV;
}

exit(0);
