#!/bin/bash

for file in *.grb; do
    cdo -f nc4 copy $file "$(basename "$file" .grb).nc"
done

