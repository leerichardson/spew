#!/bin/bash 

#PBS -l walltime=14:00:00
#PBS -j oe

# Spew commands ---
echo "Calling SPEW"

# Load required modules for R-dependencies to work
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
