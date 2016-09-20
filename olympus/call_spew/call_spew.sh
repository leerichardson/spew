#!/bin/bash

# Set the input variables 
spew_dir=/mnt/beegfs1/data/shared_group_data/syneco
base_dir=$spew_dir/$1
data_group=$2
eco_name=$(basename $base_dir)

# Make sure the base directory exists. If not, exit
if [ ! -d "$base_dir" ]; then
  echo "Directory doesn't exist!"
  exit
fi

# Set the location of the logfile 
current_date=$(date +%m%d%y)
mkdir -p $base_dir/logfiles
output_log="$base_dir/logfiles/logfile.$current_date.$eco_name.txt"

echo "Directory: $spew_dir/$base_dir"
echo "Ecosystem: $eco_name"
echo "Logfile location: $output_log"

# Submit the state level job to Olympus!
qsub -o $output_log -e $output_log -v base_dir=$base_dir,data_group=$data_group -N $eco_name /mnt/beegfs1/data/shared_group_data/syneco/olympus/call_spew/spew.sh
