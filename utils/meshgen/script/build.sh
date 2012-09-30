#!/bin/bash
if [ "$1" == "" ];
then
    echo "sorry this build.sh always requires a config argument and it is the compiler name too"
else

    nprocs=$(grep '^processor' /proc/cpuinfo | wc -l)

    #check for makefile and regenerate if doesn't exist
    if [ -e "build/Makefile" ];
    then
	echo "*** building with existing build/Makefile ***"
	cd build && make -j$nprocs meshgen.o && \
	  $1 *.o -o ../../../bin/tap-meshgen.exe
    else
	echo "*** creating new makefile ***"
	script/config.sh $1 && cd build && make -j$nprocs meshgen.o && \
	  $1 *.o -o ../../../bin/tap-meshgen.exe
    fi

fi
