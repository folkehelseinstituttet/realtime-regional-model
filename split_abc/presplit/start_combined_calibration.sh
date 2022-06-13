#!/bin/bash -l

counties=(county03)

for county in "${counties[@]}";do
	sbatch --job-name=mar$county --export=ALL,county_code=$county sep_county_template.sbatch

done;
