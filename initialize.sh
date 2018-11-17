#!/bin/bash

# Download Magnetic Field Maps
mkdir map_directory
cd map_directory
wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyHybrid_rm_3.0.txt map_directory/
wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyUpstream_rm_1.1.txt map_directory/
wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyUpstream_rm_1.1_noscale.txt map_directory/
chmod 664 blocky*
cd ..

# Build remoll
mkdir build
cd build
cmake ..
make
cd ..

# Initialize Analyses
# cp -p include/remolltypes.hh analysis/include/
# cp -p build/libremoll.so analysis/
# mkdir analysis/build
# cd analysis/build
# cmake ..
# make
