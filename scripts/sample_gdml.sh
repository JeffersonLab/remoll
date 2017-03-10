#!/bin/bash

# libmath-random-perl required

# Sample stream
sample_stream ()
{
  # Read lines
  while read line ; do
    # Do nothing, just pass through
    # here be implementation details

    # floating point regex
    fl="[-+]?[0-9]*\.?[0-9]+"
    # gdml parameter regex
    re="random_distribution\(($fl),($fl)\)"

    # Gaussian: "random_normal(mean,sigma)"
    thisre=${re/distribution/normal}
    if [[ "$line" =~ $thisre ]] ; then
      mean=${BASH_REMATCH[1]}
      sigma=${BASH_REMATCH[2]}
      value=`perl -e "use Math::Random; print Math::Random::random_normal(1,$mean,$sigma);"`
      echo "$value" 1>&2
      echo "$line" | sed -r "s/$thisre/$value/"
      continue
    fi

    # Uniform: "random_uniform(mean,half)"
    thisre=${re/distribution/uniform}
    if [[ "$line" =~ $thisre ]] ; then
      mean=${BASH_REMATCH[1]}
      half=${BASH_REMATCH[2]}
      value=`perl -e "use Math::Random; print Math::Random::random_uniform(1,$mean-$half,$mean+$half);"`
      echo "$value" 1>&2
      echo "$line" | sed -r "s/$thisre/$value/"
      continue
    fi

    # Nothing recognized
    echo "$line"
  done
}


sample_files () {
  # Loop over arguments
  while [ $# -gt 0 ] ; do
    arg=$1
    shift

    # Check if file exists
    if [ ! -e $arg ] ; then
      echo "skipping non-existing entry $arg"
      continue
    fi

    # Recurse into directories
    if [ -d $arg ] ; then
      sample_files $arg/*
      continue
    fi

    # Process files
    if [ -f $arg ] ; then
      # Sample arg as stream
      echo "sampling $arg..."
      mkdir -p `dirname $DIR/$arg`
      cat $arg | sample_stream > $DIR/$arg 2>> $DIR/sample_gdml.vars
      continue
    fi

    # Unknown
    echo "what's with $arg?"

  done
}

# Read from stdin and output to stdout if $# -eq 0
if [ $# -eq 0 ] ; then
  sample_stream
# Else parse files one by one
else
  # Determine output directory
  DIR=`mktemp -d sample_gdml.XXXXXX`
  echo "output in $DIR"
  sample_files $*
fi
