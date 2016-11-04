#!/bin/bash 

# PBS Directives ------------------------------

# Request 1 node, 64 cores, and 200 GB of memory 
#PBS -l nodes=1:ppn=56 -l mem=200gb 

# Request a maximum of 3 days for the job 
#PBS -l walltime=24:00:00

# Combine stdout and stderr into the same directory 
#PBS -j oe
	
# Spew commands -------------------------------

# Make sure we are using the most recent version of R
# and the input output module to access the $WORK directory
module load io
module load pandoc
module load r/3.2.1
module load geos 

# Remove the old outputs
rm -r $base_dir/output

# Run SPEW to generate synthetic ecosystems 
echo "Running on node: " $HOSTNAME
Rscript /mnt/beegfs1/data/shared_group_data/syneco/olympus/call_spew/run_spew.R ${base_dir} ${data_group} ${parallel_type}
