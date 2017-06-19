#!/usr/bin/env bash

# Loop through folders in us directory

echo "Run IPUMS-I"
jj=1

for i in $(find /mnt/lustre0/data/shared_group_data/syneco/spew_1.2.0/ -mindepth 3 -maxdepth 3 -type d)
do
    iso=$(basename $i)
    if [[ $iso == "usa" ]] || [[ $iso == "can" ]]; then
	echo "$iso has custom diags to run"
    elif [[ $iso == "ind" ]] || [[ $iso == "chn" ]]; then
	echo "Making diags for $iso"
	echo "requesting large node"
	qsub ~/spew/olympus/reports/qsub_ipums_large.sh -v COUNTRYPATH=$i -N "ipums$iso"
    else
	if [ -d "$i/output" ]; then
	   jj=$((jj+1))
	   echo "output exists here"
	   echo "Making diags for $iso"
	   echo "$i"
	   qsub ~/spew/olympus/reports/qsub_ipums.sh -v COUNTRYPATH=$i -N "ipums_$iso"
	fi
    fi

done

echo "We should have $jj ipums total diags"

