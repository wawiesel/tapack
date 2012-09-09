#!/bin/bash
echo
echo '*****************************'
echo '***TAPACK REGRESSION SUITE***'
echo '*****************************'
echo

fmt="%25s %10s %10s %10s \n";
printf "$fmt" "file" "RESULT" "NPASS" "NFAIL"

for t in tests/*.cntl;
do
     printf "$fmt" $(perl script/test-single.pl $t)
done
