#!/bin/bash 

# PBS Directives ---

# Request 2 nodes, 63 cores, and 200 GB of memory 
#PBS -l nodes=2:ppn=32 -l mem=200gb 

# Request a maximum of 3 days for the job 
#PBS -l walltime=24:00:00

# Combine stdout and stderr into the same directory 
#PBS -j oe
	
# Spew commands ---
echo "Calling SPEW"

# Make sure we are using the most recent version of R
# and the input output module to access the $WORK directory
module load r/3.3.1
module load io
module load gdal
module load geos 
module load pandoc
module load openmpi/1.10.0

echo "Modules: "
module list 

# Remove the old outputs
echo "Removing old outputs at: $output_dir"
rm -r $output_dir

# Run SPEW to generate synthetic ecosystems 
echo "Running on node: " $HOSTNAME
spew_script=/mnt/beegfs1/data/shared_group_data/syneco/spew/olympus/call_spew/run_spew.R

mpirun -n 1 --oversubscribe --mca pml ob1 --hostfile $PBS_NODEFILE --prefix /usr/lib64/openmpi Rscript --no-save $spew_script ${input_dir} ${output_dir} ${data_group} ${run_type}

echo "Job Finished!"