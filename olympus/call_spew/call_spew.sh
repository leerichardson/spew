#!/bin/bash

# Construct all relevant file-paths ---
spew_input=/mnt/beegfs1/data/shared_group_data/syneco/spew_input
spew_output=/mnt/beegfs1/data/shared_group_data/syneco/spew_1.3.0

input_dir=$spew_input/$1/input
echo "Input Directory: $input_dir"

if [ ! -d "$input_dir" ]; then
  echo "Input directory does not exist!"
  exit
fi

output_dir=$spew_output/$1/output
echo "Output Directory: $output_dir"

logfile_dir=$spew_output/$1/logfiles
if [ ! -d "$logfile_dir" ]; then
  echo "Creating logfile directory at: $logfile_dir"
  mkdir -p $logfile_dir  
fi
echo $logfile_dir

eco_name=$(basename $1)
echo $eco_name
current_date=$(date +%m%d%y)
output_log="$logfile_dir/logfile.$current_date.$eco_name.txt"
rm $output_log
echo "Logfile Location: $output_log"

data_group=$2
echo "Data Group: $data_group"

nodes=1
ppn=5
if [ -z "$4" ]; then 
	queue=batch
else
	queue=$4
fi

run_type=$3
if [ -z "$3" ]; then 
	run_type="SOCK"
fi
echo "Run Type: $run_type"

# Use mpirun script if using MPI. If not, call the normal script
if [ $run_type = "MPI" ]; then 
	call_script=/mnt/beegfs1/data/shared_group_data/syneco/spew/olympus/call_spew/spew_mpi.sh
	ppn=63
	
	if [ -z "$5" ]; then 
		nodes=2
	else
		nodes=$5
	fi
else
	call_script=/mnt/beegfs1/data/shared_group_data/syneco/spew/olympus/call_spew/spew.sh
	ppn=55
fi

size="gb"
mempn=7
memtot=$(($mempn*$nodes*$ppn))

echo "Calling Script: $call_script. Nodes: $nodes, PPN: $ppn, Queue: $queue, Mem Per Node: $mempn$size Total Memory: $memtot$size"

qsub -o $output_log -e $output_log -v input_dir=$input_dir,output_dir=$output_dir,data_group=$data_group,run_type=$run_type -N $eco_name $call_script -l nodes=$nodes:ppn=$ppn -q $queue -l mem=882gb # -l pmem=${mempn}gb 
