#!/bin/bash

module load cmake 3.5.1

# Download Magnetic Field Maps
#mkdir map_directory
#cd map_directory
#wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyHybrid_rm_3.0.txt map_directory/
#wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyUpstream_rm_1.1.txt map_directory/
#wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/blockyUpstream_rm_1.1_noscale.txt map_directory/
#wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/hybridJLAB.txt
#wget http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/upstreamJLAB_1.25.txt
#chmod 664 blocky*
#chmod 664 *JLAB*
#cd ..

# Build remoll
mkdir build
cd build
cmake ..
make
make install
cd ..

# Initialize Analyses
mkdir analysis/build
cd analysis/build
cmake ..
make
make install
