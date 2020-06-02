
# ensure we have PS1 set
PS1="Singularity $SINGULARITY_NAME:\\w> "
export PS1

LD_LIBRARY_PATH=/.singularity.d/libs
export LD_LIBRARY_PATH

PATH=/usr/local/bin:/usr/bin:/bin
export PATH

