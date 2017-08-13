#!/usr/bin/perl

use warnings;
use strict;

die "Please specify the cache directory!\n" if !$ARGV[0];

chdir $ARGV[0] || die "Could not enter the cache directory!";

my @info = stat("private");

mkdir "packages";
mkdir "headers";
chown $info[4], $info[5], "packages", "headers";

for my $fname (glob('*.deb *pgp *gz *bz2 *Release')) {
   my $data=0;
   my $size=0;
   open(my $in, '<', $fname);
   open(my $daten, '>', "packages/$fname");
   open(my $header, '>', "headers/$fname");
   while(<$in>) {
      if($data) { print $daten $_; next; };
      s/\r$//; # Some combined files have /r/n terminated headers. See bug
               # 355157. Not needed in new split format.
      print $header $_;
      $size=$1 if /^Content-Length: (\d+)/;
      $data=1 if /^$/;
   }
   close($daten);
   close($header);
   if (!$data) {
       print "Not found header/data boundary in file $fname. Skipping\n";
       unlink "packages/$fname", "headers/$fname";
       next;
   }
   my @statinfo = stat("packages/$fname");
   if($size == $statinfo[7]) {
      chown $info[4], $info[5], "packages/$fname", "headers/$fname";
      utime $statinfo[9], $statinfo[9], "packages/$fname", "headers/$fname";
      print "Processed $fname.\n";
      unlink $fname;
   }
   else {
      unlink "packages/$fname";
      unlink "headers/$fname";
   }
}
