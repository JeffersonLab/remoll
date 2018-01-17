FROM jeffersonlab/jlabce:2.1

ENV JLAB_VERSION=2.1
ENV JLAB_ROOT=/jlab
ENV JLAB_SOFTWARE=/jlab/2.1/Linux_CentOS7.3.1611-x86_64-gcc4.8.5

ENV REMOLL=$JLAB_SOFTWARE/remoll

WORKDIR $REMOLL
# Compile remoll
# RUN git clone https://github.com/jeffersonlab/remoll $REMOLL
ADD . .
RUN source $JLAB_ROOT/$JLAB_VERSION/ce/jlab.sh && \
    mkdir -p $REMOLL/build && cd $REMOLL/build && cmake .. && make

# Download the map data files and place them in the correct directory
RUN wget -r --no-parent -l1 -A txt http://hallaweb.jlab.org/12GeV/Moller/downloads/remoll/
RUN mkdir map_directory && \
    find ./hallaweb.jlab.org -mindepth 2 -type f -exec mv -t ./map_directory -i '{}' + && \
    rm -rf hallaweb.jlab.org

# Create entry point bash script
RUN echo '#!/bin/bash'                                > /entrypoint.sh && \
    echo 'unset OSRELEASE'                            >> /entrypoint.sh && \
    echo 'source $JLAB_ROOT/$JLAB_VERSION/ce/jlab.sh' >> /entrypoint.sh && \
    echo 'cd $REMOLL && exec ./build/remoll $1'       >> /entrypoint.sh && \
    chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
CMD ["macros/runexample.mac"]

