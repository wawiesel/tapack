#!/usr/bin/perl
use warnings;
use strict;
use File::Basename qw(dirname basename);

my $file=$ARGV[0];
if( ! $file=~m/\.cntl$/ ){
    die("tests must have a .cntl extension");
}
my $base=basename($file,'.cntl');
my $dir="build/tests/$base";

#get local copy
`rm -rf $dir`;
`mkdir -p $dir`;
`cp $file $dir`;

$|=1;

my $NPASS=0;
my $NFAIL=0;
my $FIN=0;
my $MSG;

if( ! -e "bin/tap.exe" ){
    die("need to run script/build.sh");
}

`bin/tap $dir/$base.cntl &>$dir/$base.mlog`;

$NPASS=0;
$NFAIL=0;
open(LOG,"<$dir/$base.log");
while(<LOG>){
    if( m/\[test=PASS\]/ ){
        $NPASS++;
    } elsif( m/\[test=FAIL\]/ ){
        $NFAIL++;
    } elsif( m/\[\[TAP\]\] successfully shutdown\./ ){
        $FIN=1;
    }
}    
close(LOG);

my $CHECK='';
if( $FIN != 1 ){
    $NFAIL++;
    $CHECK="[check log]";
}
$MSG=($NPASS>0 && $NFAIL==0)?'PASS':'FAIL';
printf "%s %s %s %s %s\n",$file,$MSG,$NPASS,$NFAIL,$CHECK;

