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

FROM jeffersonlab/jlabce:2.3-mt

# Install libgcj and pdftk
RUN wget -q https://copr.fedorainfracloud.org/coprs/robert/gcj/repo/epel-7/robert-gcj-epel-7.repo -P /etc/yum.repos.d && \
    wget -q https://copr.fedorainfracloud.org/coprs/robert/pdftk/repo/epel-7/robert-pdftk-epel-7.repo -P /etc/yum.repos.d && \
    yum install -q -y pdftk ghostscript

# Add Tini entry point
ENV TINI_VERSION v0.19.0
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /tini
RUN chmod +x /tini

# Set JLab CE version
ENV JLAB_VERSION=2.3
ENV JLAB_ROOT=/jlab

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

# Environment through /etc/profile
RUN ln -sf $REMOLL/bin/remoll.csh /etc/profile.d/remoll.csh
RUN ln -sf $REMOLL/bin/remoll.sh /etc/profile.d/remoll.sh

# Override JLab CE environment for container use
COPY docker/jlab.sh /jlab/${JLAB_VERSION}/ce/jlab.sh

# Entry point loads the environment
ENTRYPOINT ["/tini", "--", "bash", "-c", "source /etc/profile && \"$@\"", "-s"]

CMD ["remoll"]
