#!/usr/bin/env bash

awk -v FS="," '{ printf "https://rest.soilgrids.org/soilgrids/v2.0/properties/query?lon=%s&lat=%s&property=nitrogen&depth=15-30cm&value=mean\n", $1, $2 }' ../plots_coord.csv > curlget

while IFS="" read -r p || [ -n "$p" ]; do
	curl -k -X GET $p -H "accept: application/json" | jq '.properties.layers | .[] | .depths | .[] | .values.mean'
done < curlget > ../data/nitrogen.txt

rm curlget
