#!/bin/bash 

# PBS Directives ------------------------------

# Request 2 nodes, 63 cores, and 200 GB of memory 
#PBS -l nodes=2:ppn=63 -l mem=200gb 

# Request a maximum of 3 days for the job 
#PBS -l walltime=24:00:00

# Combine stdout and stderr into the same directory 
#PBS -j oe
	
# Spew commands -------------------------------
echo "Calling SPEW"

# Load modules on both the host node AND the other nodes!
module load io 
module load geos/3.5.0
module load r/3.2.1
module load openmpi/1.8.1 
module list 

# Remove the old outputs
rm -r $base_dir/output

# Run SPEW to generate synthetic ecosystems 
echo "Running on node: " $HOSTNAME
mpirun -n 1 --hostfile $PBS_NODEFILE --prefix /usr/lib64/openmpi Rscript --no-save /mnt/beegfs1/data/shared_group_data/syneco/olympus/call_spew/run_spew.R ${base_dir} ${data_group} ${parallel_type}
