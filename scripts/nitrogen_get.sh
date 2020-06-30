#!/usr/bin/env bash

curlget=$(awk -v FS="," '{ printf "https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lon=%s&lat=%s&property=nitrogen&depth=15-30cm&value=mean\n", $1, $2 }' plots_coord.csv > curlget)

for line in curlget; do 
	curl -k -X GET $line -H "accept: application/json" | jq '.properties.layers | .[] | .depths | .[] | .values.mean'
done > ../data/nitrogen 


