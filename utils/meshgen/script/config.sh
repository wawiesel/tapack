#!/bin/bash

################################
#configuration for this project
################################
exe="../../bin/tap-meshgen.exe";
################################

config=$1;
mkdir -p build;

#get template to use mkmf
if [ ! "$config" == "" ];
then
    config_file="config/$config.mkmf.template";
    if [ ! -e "$config_file" ];
    then
        echo "ERROR: configuration {$config} specified does not exist at {$config_file}"
        exit 1;
    fi
else
    echo "ERROR: you must specify the configuration name as the only argument, e.g. "
    echo "       config={ifort} corresponding to "
    echo "       file={config/ifort.mkmf.template}"
    exit 2
fi  

echo "****************************************************************"
echo "****************************************************************"
echo "*** configuring by running mkmf to creating build/Makefile for" 
echo "***   executable exe={$exe}"
echo "***   configuration name {$config}"
echo "***   configuration file {$config_file}"
echo "***"
echo "*** build using either script/build.sh or cd build && make"
echo "****************************************************************"
echo "****************************************************************"



#last source is global source from TAPACK and because we need to build
#the exe special, we don't include it here
cd build && ../script/mkmf -t ../$config_file ../src ../../../src
