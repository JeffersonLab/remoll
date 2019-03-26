FROM jeffersonlab/jlabce:2.3

# Install libgcj and pdftk
RUN wget https://copr.fedorainfracloud.org/coprs/robert/gcj/repo/epel-7/robert-gcj-epel-7.repo -P /etc/yum.repos.d && \
    wget https://copr.fedorainfracloud.org/coprs/robert/pdftk/repo/epel-7/robert-pdftk-epel-7.repo -P /etc/yum.repos.d && \
    yum install -y pdftk

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

# Create entry point bash script
RUN echo '#!/bin/bash'                                >  /usr/local/bin/entrypoint.sh && \
    echo 'unset OSRELEASE'                            >> /usr/local/bin/entrypoint.sh && \
    echo 'source $JLAB_ROOT/$JLAB_VERSION/ce/jlab.sh' >> /usr/local/bin/entrypoint.sh && \
    echo 'export PATH=${REMOLL}/bin:${PATH}'          >> /usr/local/bin/entrypoint.sh && \
    echo 'export REMOLL=${REMOLL}'                    >> /usr/local/bin/entrypoint.sh && \
    echo 'cd $REMOLL && exec $*'                      >> /usr/local/bin/entrypoint.sh && \
    chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

