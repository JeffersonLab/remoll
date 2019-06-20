FROM jeffersonlab/jlabce:2.3

# Install libgcj and pdftk
RUN wget https://copr.fedorainfracloud.org/coprs/robert/gcj/repo/epel-7/robert-gcj-epel-7.repo -P /etc/yum.repos.d && \
    wget https://copr.fedorainfracloud.org/coprs/robert/pdftk/repo/epel-7/robert-pdftk-epel-7.repo -P /etc/yum.repos.d && \
    yum install -y pdftk ghostscript

# Create entry point bash script
RUN echo '#!/bin/bash'                                >  /entrypoint.sh && \
    echo 'unset OSRELEASE'                            >> /entrypoint.sh && \
    echo 'source /jlab/2.3/ce/jlab.sh'                >> /entrypoint.sh && \
    echo 'export PATH=/jlab/remoll/bin:${PATH}'       >> /entrypoint.sh && \
    echo 'export REMOLL=/jlab/remoll'                 >> /entrypoint.sh && \
    echo 'cd /jlab/remoll && exec "$@"'               >> /entrypoint.sh && \
    chmod +x /entrypoint.sh

# Compile remoll
ADD . /jlab/remoll
RUN source /entrypoint.sh && \
    mkdir -p $REMOLL/build && \
    pushd $REMOLL/build && \
    cmake .. && \
    make -j$(nproc) && \
    make install

ENTRYPOINT ["/entrypoint.sh"]

CMD ["build/remoll","-h"]

