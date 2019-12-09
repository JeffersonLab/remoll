#!/bin/bash

shopt -s nullglob

# Exit whenever non-zero exit code occurs
set -euo pipefail

# Determine absolute path of this script
dir=`dirname $0`/../..
dir=`readlink -f ${dir}`

# The test suite can be specified as first argument, default is "commit"
suite="${1:-commit}"

# The branch name is used to avoid clobbering comparative output
branch=`git rev-parse --abbrev-ref HEAD`

# Set test suite output directories
rootfiles="rootfiles/tests/${suite}/${branch}"

# Create a transparent stamp and send to stdout
function stamp() {
ps2pdf -sPAPERSIZE=letter - - <<EOF
%!PS
/cm { 28.4 mul } bind def
/draft-Bigfont /Helvetica-Bold findfont 12 scalefont def
/draft-copy {
        gsave initgraphics 0.5 setgray
        1 cm 1 cm moveto
        draft-Bigfont setfont
        ($1) show grestore
 } def
draft-copy showpage
EOF
}

# Loop over arguments assumed to be directories
while [ $# -gt 0 ] ; do
  # Stamp all pdf files in directory
  for file in ${rootfiles}/analysis/*.pdf ; do
    stamp $file | pdftk - stamp $file output $file.new && mv $file.new $file
  done
  # Concatenate them all
  name=${rootfiles##*tests/}
  name=${name//\//_}_analysis_book
  pdftk ${rootfiles}/analysis/*.pdf cat output rootfiles/${name}.pdf
  shift
done
