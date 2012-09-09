#!/bin/bash

#check for makefile and regenerate if doesn't exist
if [ -e "build/Makefile" ];
then
    echo "*** building with existing build/Makefile ***"
    cd build && make
else
    echo "*** creating new makefile ***"
    script/config.sh $1 && cd build && make
fi
