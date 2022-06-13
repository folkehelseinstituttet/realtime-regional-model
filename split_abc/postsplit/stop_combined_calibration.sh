#!/bin/bash -l

counties=(county03)

for county in "${counties[@]}";do
	scancel --name=mar$county

done;
