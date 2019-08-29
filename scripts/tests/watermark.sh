#!/bin/bash

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
  for file in $1/*.pdf ; do
    stamp $file | pdftk - stamp $file output $file.new && mv $file.new $file
  done
  # Concatenate them all
  pdftk $1/*.pdf cat output ${1//\//_}.pdf
  shift
done
