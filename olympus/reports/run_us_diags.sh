#!/usr/bin/env bash

# Loop through folders in us directory

echo "test"


for dir in /mnt/beegfs1/data/shared_group_data/syneco/input/west/north_america/united_states/* ; do
    [ -d "${dir}" ] || continue # if not a directory, skip
    dirname="$(basename "${dir}")"
    ## Clear old report
    rm reports/$dirname/*
    if [[ $dirname = *[0-9] ]]; then
    	echo "Making qsub for $dirname";
    	## California or Texas needs the large node
    	if [[ $dirname = "48" ]] || [[ $dirname = "06" ]]; then
    	    echo "requesting large node"
    	    qsub qsub_us_large.sh -v FIPS=$dirname -N "us_$dirname"
    	else
    	    qsub qsub_us.sh -v FIPS=$dirname -N "us_$dirname"
    	fi
    fi

done
