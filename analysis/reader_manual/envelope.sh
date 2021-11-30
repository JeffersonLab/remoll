#!/bin/bash

if [ -e test.png ]
then
    rm test.png
fi
loud=0
name="$1"
let len=${#name}-5
fname=${name:0:$len}
for value in {6000..14000..200}
do
    ./reader $name $value $value 1 10 10 6
    if [ -e test.png -a $loud -ne 0 ]
    then
        rm test.png
        pngName="${fname}_${value}.png"
        display $pngName &   
    fi
    echo "Done $value"
done

for value in {14000..33000..200}
do
    ./reader $name $value $value 1 10 50 6
    if [ -e test.png -a $loud -ne 0 ]
    then
        rm test.png
        pngName="${fname}_${value}.png"
        display $pngName &   
    fi
    echo "Done $value"
done
