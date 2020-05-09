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

FROM jeffersonlab/jlabce:2.3-mt

# Install libgcj and pdftk
RUN wget -q https://copr.fedorainfracloud.org/coprs/robert/gcj/repo/epel-7/robert-gcj-epel-7.repo -P /etc/yum.repos.d && \
    wget -q https://copr.fedorainfracloud.org/coprs/robert/pdftk/repo/epel-7/robert-pdftk-epel-7.repo -P /etc/yum.repos.d && \
    yum install -q -y pdftk ghostscript

ENV JLAB_VERSION=2.3
ENV JLAB_ROOT=/jlab
ENV JLAB_SOFTWARE=/jlab/2.3/Linux_CentOS7.2.1511-x86_64-gcc4.8.5

ENV REMOLL=/jlab/remoll

WORKDIR $REMOLL

# Compile remoll
ADD . .
RUN source $JLAB_ROOT/$JLAB_VERSION/ce/jlab.sh && \
    mkdir -p $REMOLL/build && \
    pushd $REMOLL/build && \
    cmake .. && \
    make -j$(nproc) && \
    make install

# Create environment point bash script
RUN echo '#!/bin/bash'                                >  /entrypoint.sh && \
    echo 'unset OSRELEASE'                            >> /entrypoint.sh && \
    echo 'source $JLAB_ROOT/$JLAB_VERSION/ce/jlab.sh' >> /entrypoint.sh && \
    echo 'export PATH=${REMOLL}/bin:${PATH}'          >> /entrypoint.sh && \
    echo 'export REMOLL=${REMOLL}'                    >> /entrypoint.sh && \
    echo 'cd $REMOLL && exec "$@"'                    >> /entrypoint.sh && \
    chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

CMD ["build/remoll","-h"]
