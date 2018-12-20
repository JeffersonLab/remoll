#!/bin/bash

if [ -e test.png ]
then
    rm test.png
fi
name="$1"
let len=${#name}-5
fname=${name:0:$len}
for value in {6000..30000..500}
do
    ./reader $name $value
    if [ -e test.png ]
    then
        rm test.png
        pngName="${fname}_${value}.png"
        display $pngName &   
    fi
    echo "Done $value"
done


