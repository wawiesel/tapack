#!/usr/bin/perl
use strict;
use warnings;

my $base=$ARGV[0]; #e.g. azmy0-mla0-d4
my $pct=$ARGV[1]; 
$pct||="50";
if( !@ARGV ){
  print "
usage: tap-get-cell-refinement-list.pl <base> <pct> > refined.inp

  This script writes out \\MSHsplit{cell}{QUAD} cards corresponding to the
  the cells with the most error, where error is defined as difference between
  the high-order and low-order solutions.

example: tap-get-cell-refinement-list.pl azmy0-mla-d4 50 

  Would look in the current directory for
    azmy0-mla-d4-LO_ScalarFluxC.dat
    azmy0-mla-d4-ScalarFluxC.dat
  and then determine the top 50% error cells and write 
  \\MSHsplit commands for the proper cells to STDOUT

";
  exit 1;
}

my $lo="$base-LO_ScalarFluxC.dat";
my $hi="$base-ScalarFluxC.dat";

if( ! (-e $lo && -e $hi) ){
  die("must provide valid lo={$lo} and hi={$hi} files");
}

#get the lines corresponding to each cell
my @lo=split("\n",`tail -n +3 $lo`);
my @hi=split("\n",`tail -n +3 $hi`);
my @rel;

print STDOUT "
!!! 
!!! difference between HI and LO cell-average solutions
!!!
";
printf STDOUT "!!! %5s %19s %19s %19s\n","cell","HI","LO","reldiff";
for my $i (0..$#lo){
  my $diff=($hi[$i]-$lo[$i])/(0.5*($hi[$i]+$lo[$i]));
  printf STDOUT "!!! %5d %19.12e %19.12e %19.12e\n",$i+1,$hi[$i],$lo[$i],$diff;
  push @rel,abs($diff);
}

print STDOUT "
!!! 
!!! sorted in descending order
!!!
";
my @order = sort {$rel[$b] cmp $rel[$a]} (0..$#rel);
printf STDOUT "!!! %5s %5s %19s\n","order","cell","|reldiff|";
for my $i (0..$#lo){
  printf STDOUT "!!! %5d %5d %19.12e\n",$i+1,$order[$i]+1,$rel[$order[$i]];
}


print STDOUT "
!!! 
!!! cell refinement cards for $pct% of cells with the most error (others commented out)
!!!
";
print STDOUT "!!! rename in order to have cell names corresponding to cell numbers\n";
print STDOUT "\\MSHrename{T}{T}{T}\n";
my $first=1;
for my $i (0..$#lo){
  if( 100*($i+1)/($#lo+1) > $pct ){
    if( $first){print STDOUT "!!! cutoff for $pct% of cells\n";}
    $first=0;
    print STDOUT "!!! ";
  }
  printf STDOUT "\\MSHsplit{c%d}{QUAD}\n",$order[$i]+1;
}
printf STDOUT "\\MSHfinalize{T}\n";
