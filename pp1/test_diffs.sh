#!/bin/bash

for ext in frag decaf; do 
    for file in samples/*.$ext; do
        correct_output=$(echo $file | sed "s/$ext/out/g")
        out="$(diff -w <(./dcc < $file 2>&1) $correct_output)"
        ret=$?
        if ! [[ $ret =~ 0 ]]; then
            echo $file
            echo "$out"
        fi
    done 
done 

