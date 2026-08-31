# Instructions for building remoll, a Docker image for the MOLLER experiment.
#
# Instructions for building the remoll image: 
#   docker build -t jeffersonlab/remoll:latest .
# Instructions for building the remoll image without cache: 
#   docker build --no-cache -t jeffersonlab/remoll:latest .
# Sharing the remoll image on DockerHub: 
#   docker push jeffersonlab/remoll:latest
#
# Running the container with docker:
#   docker run --rm -it jeffersonlab/remoll:latest remoll macros/runexample.mac
#
# Running the container with singularity:
#   singularity build --disable-cache --fix-perms --sandbox remoll:latest docker-daemon://jeffersonlab/remoll:latest
#   singularity run remoll:latest remoll macros/runexample.mac
# Note: building a sandbox image may not work on all filesystem or on
# hyperlinked directories. Make sure you are in an actual directory with:
#   cd `readlink -f .`
#

FROM jeffersonlab/remoll-builder:main

# XrootD
RUN yum -y install python3-pip \
    xrootd-client

# Stashcp
RUN pip3 install setuptools && \
    pip3 install stashcp

# Set remoll location
ENV REMOLL=/jlab/remoll

# Compile remoll
WORKDIR $REMOLL
COPY . .
RUN source /etc/profile && \
    mkdir -p $REMOLL/build && \
    pushd $REMOLL/build && \
    cmake .. && \
    make -j$(nproc) && \
    make install && \
    make clean

# Entry point loads the environment
ENTRYPOINT ["/bin/bash", "-c", "source /etc/profile && source $REMOLL/bin/remoll.sh && \"$@\"", "--"]
CMD ["remoll"]
