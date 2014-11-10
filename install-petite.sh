#!/bin/bash
set -x
wget http://www.scheme.com/download/pcsv8.4-a6le.tar.gz

tar xzf pcsv8.4-a6le.tar.gz

cd csv8.4/custom
dir=`pwd`
./configure --installprefix="$dir"/../..
make
make install
