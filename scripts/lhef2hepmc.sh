#!/bin/bash

if [ $# -ne 2 ] ; then
  echo "Usage: `basename $0` input.lhe.gz output.hepmc"
  exit 0
fi

zcat $1 | sed 's/nan/0.0/g' | lhef2hepmc > $2
