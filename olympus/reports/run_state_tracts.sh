#!/usr/bin/env bash

# Loop through folders in us directory

fips="$1"
echo "Looping over tracts for $fips"

path=/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa/$fips/output

counter=0
for dir in $path/* ; do
    [ -d "${dir}" ] || continue # if not a directory, skip
    dirname="$(basename "${dir}")"
    if [[ $dirname = output_* ]]; then
	for ff in $dir/eco/* ; do
	    bn=$(basename $ff)
	    if [[ $bn = household* ]]; then
		tract=$(echo "$bn" | cut -d'_' -f2 | cut -d'.' -f1)
		counter=$((counter+1))
		echo "Making diags for $tract"
		## run in parallel on cores
		~/spew/olympus/reports/make_tract_report.sh "$dir/eco" "$tract" "$fips" &
		if  ! ((counter % 16)) ; then
		    echo "$counter"
		    wait
		fi
	    fi
	done
    fi
done

wait
echo "$counter tracts in state"

