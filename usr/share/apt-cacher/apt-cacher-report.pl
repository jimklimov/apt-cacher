#!/usr/bin/perl

# apt-cacher-report.pl
# Script to generate usage reports for the Apt-cacher package caching system.
#
# Copyright (C) 2002,2004 Jonathan Oxer <jon@debian.org>
# Distributed under the terms of the GNU Public Licence (GPL).

use strict;
use warnings;
#############################################################################
### configuration ###########################################################
# Include the library for the config file parser
require('/usr/share/apt-cacher/lib/apt-cacher.pl');
use POSIX qw(strftime);


# Read in the config file and set the necessary variables
my $configfile = '/etc/apt-cacher/apt-cacher.conf';

while(scalar (@ARGV)) {
    my $arg=shift(@ARGV);
    if($arg eq '-c') {
	$configfile=shift(@ARGV) || die '-c option requires an argument';
	die "$configfile not a file" if ! -f $configfile;
	die "$configfile unreadable" if ! -r $configfile;
    }
    elsif($arg eq '-h' || $arg eq '--help') {
	print <<EOM;
USAGE: $0 <options>
Options:

-c configfile   Custom config file (default: $configfile)
-h|--help	Print this help message
EOM
	exit(0);
    }
    else {
	die "Unknown parameter $arg\n";
    }
}

# Needs to be global for setup_ownership()
our $cfg = eval{ read_config($configfile) };

# not sure what to do if we can't read the config file...
die "Could not read config file: $@" if $@;

# check whether we're actually meant to generate a report
if ( $cfg->{generate_reports} ne 1 ){
	exit 0;
}

#Give up root
setup_ownership();

# Now set some things from the config file
# $logfile used to be set in the config file: now we derive it from $log_dir
my $logfile = "$cfg->{log_dir}/access.log";


###################################################
# Read in the logfiles if they exist, from oldest to newest

# First we look for rolled and compressed logfiles, from
# /var/log/apt-cacher/access.log.12.gz to access.log.2.gz
my $logcount = 12;
my @logdata;
while ($logcount > 1)
{
	if (-f "${logfile}.$logcount.gz") {
		my $logdataraw = `zcat ${logfile}.$logcount.gz`;
		push (@logdata, split("\n", $logdataraw));
	}
	$logcount--;
}

# Then the immediately rolled (but uncompressed) log
if (-f "${logfile}.1") {
	open(my $lfh, '<', "${logfile}.1");
	push(@logdata, <$lfh>);
	close($lfh);
}

# Then finally the current working log
if (-f "${logfile}") {
	open(my $lfh, '<', $logfile);
	push(@logdata, <$lfh>);
	close($lfh);
}

#read current time
#($second,$minute,$hour,$day,$month,$year,$null,$null,$null)=localtime(time);
my $datetime = strftime("%Y-%m-%d %H:%M:%S", localtime());

#$year = $year + 1900;
#$month=$month + 1;

my $hit_count = 0;
my $hit_bytes = 0;
my $miss_count = 0;
my $miss_bytes = 0;
my ($firstrecord,$lastrecord);

#parse logfile:
foreach (@logdata)
{
	my @line = split /\|/;
	if ($#line==4) { # Old format without PID
	    splice @line, 1, 0, ''; # Insert empty field
	}

	next unless $#line == 5; # Skip if parsing fails

	my $req_date = $line[0];
#	my $req_pid = $line[1];
#	my $req_ip   = $line[2];
	my $req_result = $line[3];
	my $req_bytes  = $line[4] =~ /^\d+$/ ? $line[4] : 0;
#	my $req_object = $line[5];

	$lastrecord = $req_date;
	if(!$firstrecord) {
		$firstrecord = $req_date;
	}
	if ( $req_result =~ /HIT|NOTMOD|HEAD/ )
	{
		$hit_count++;
		$hit_bytes += $req_bytes;
	}
	else
	{
		$miss_count++;
		$miss_bytes += $req_bytes;
	}

}

my $total_count = $hit_count + $miss_count;
my ($hit_count_percent,$miss_count_percent);

if($total_count eq 0)
{
	$hit_count_percent = 0;
	$miss_count_percent = 0;
} else {
	$hit_count_percent = (int(($hit_count / $total_count) * 10000)) / 100;
	$miss_count_percent = (int(($miss_count / $total_count) * 10000)) / 100;
}

my $total_bytes = $hit_bytes + $miss_bytes;

##################################################
# At this point we have hit/miss/total counts, and hit/miss/total traffic
# So now we need to decide what units to use for each one, and set a
# human-readable string. Displays as MB unless > 2000MB, in which case it
# displays as GB.
# Yes, I know this really should be a subroutine. Sigh. One day. Maybe.

my ($tx,$total_trafficstring,$hit_trafficstring,$miss_trafficstring,$hit_data_percent,$miss_data_percent);

if($total_bytes > 2097152000)
{
	$tx = (int(($total_bytes/1073741824) * 1000)) / 1000;
	$total_trafficstring = "$tx GB";
} else {
	$tx = (int(($total_bytes/1048576) * 1000)) / 1000;
	$total_trafficstring = "$tx MB";
}

if($hit_bytes > 2097152000)
{
        $tx = (int(($hit_bytes/1073741824) * 1000)) / 1000;
        $hit_trafficstring = "$tx GB";
} else {
        $tx = (int(($hit_bytes/1048576) * 1000)) / 1000;
        $hit_trafficstring = "$tx MB";
}

if($miss_bytes > 2097152000)
{
        $tx = (int(($miss_bytes/1073741824) * 1000)) / 1000;
        $miss_trafficstring = "$tx GB";
} else {
        $tx = (int(($miss_bytes/1048576) * 1000)) / 1000;
        $miss_trafficstring = "$tx MB";
}


##################################################
# Set percentages to 0 if no records, otherwise calculate
if($total_bytes eq 0)
{
	$hit_data_percent = 0;
	$miss_data_percent = 0;
} else {
	$hit_data_percent = (int(($hit_bytes / $total_bytes) * 10000)) / 100;
	$miss_data_percent = (int(($miss_bytes / $total_bytes) * 10000)) / 100;
}

##################################################
# If there weren't actually any logfiles processed these will be null, so we'll
# set them to strings
if(!$firstrecord)
{
	$firstrecord = "unknown";
}
if(!$lastrecord)
{
	$lastrecord = "unknown";
}

##################################################
# spit out the report
my $output = "
<html>
<title>Apt-cacher traffic report</title><style type=\"text/css\"><!--
a { text-decoration: none; }
a:hover { text-decoration: underline; }
h1 { font-family: arial, helvetica, sans-serif; font-size: 18pt; font-weight: bold;}
h2 { font-family: arial, helvetica, sans-serif; font-size: 14pt; font-weight: bold;}
body, td { font-family: arial, helvetica, sans-serif; font-size: 10pt; }
th { font-family: arial, helvetica, sans-serif; font-size: 11pt; font-weight: bold; }
//--></style>
</head>
<body>";

#	print "<html><head><title>Apt-cacher traffic report</title></head>\n";
#	print "<body bgcolor=\"#ffffff\">\n";

$output .= "<p>
<table border=0 cellpadding=8 cellspacing=1 bgcolor=\"#000000\" align=\"center\" width=\"600\">
<tr bgcolor=\"#9999cc\"><td> <h1>Apt-cacher traffic report</h1> </td></tr>
<tr bgcolor=\"#cccccc\"><td>For more information on apt-cacher visit <a href=\"http://packages.debian.org/apt-cacher\">http://packages.debian.org/apt-cacher</a>.
</td></tr>
</table>";

$output .= "<h2 align=\"center\">summary</h2>
<table border=0 cellpadding=3 cellspacing=1 bgcolor=\"#000000\" align=\"center\" width=\"600\">
<tr bgcolor=\"#9999cc\"><th bgcolor=\"#9999cc\"> Item </th><th> Value </th></tr>
<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> Report generated </td><td> $datetime </td></tr>
<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> Administrator </td><td> <a href=\"mailto:$cfg->{admin_email}\">$cfg->{admin_email}</a> </td></tr>";
$output .= "<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> First request </td><td> $firstrecord </td></tr>";
$output .= "<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> Last request </td><td> $lastrecord </td></tr>";
$output .= "<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> Total requests </td><td> $total_count </td></tr>";
$output .= "<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> Total traffic </td><td> $total_trafficstring </td></tr>";
$output .= "</table>";

$output .= "<h2 align=\"center\">cache efficiency</h2>
<table border=0 cellpadding=3 cellspacing=1 bgcolor=\"#000000\" align=\"center\" width=\"600\">
<tr bgcolor=\"#9999cc\"><th></th><th>Cache hits</th><th>Cache misses</th><th>Total</th></tr>\n
<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> Requests </td><td>$hit_count ($hit_count_percent%)</td><td>$miss_count ($miss_count_percent%)</td><td>$total_count</td></tr>\n
<tr bgcolor=\"#cccccc\"><td bgcolor=\"#ccccff\"> Transfers </td><td>$hit_trafficstring ($hit_data_percent%)</td><td>$miss_trafficstring ($miss_data_percent%)</td><td>$total_trafficstring</td></tr>\n
</table>";
	
$output .= "</body></html>\n";

#print $output;
my $report_file = "$cfg->{log_dir}/report.html";
unlink $report_file;
open(my $rfh, '>', $report_file) or die "Unable to open $report_file";
print $rfh "$output\n";
close $rfh;


exit 0;

