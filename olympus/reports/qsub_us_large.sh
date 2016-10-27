#PBS -l walltime=24:00:00   #!/bin/bash                                                                                          
# PBS Directives ----                                                                                
# Combine stoud and stderr into the same directory                   
#PBS -j oe

# Request the 1 TB node
#PBS -l nodes=l001.olympus.psc.edu:ppn=64 
#PBS -l mem=114470
#PBS -l pmem=114470

# Request a specific amount of time for the job...                   
#PBS -l walltime=5:00:00                                                                            





# Getting data                                                       

ml r/3.2.1
ml pandoc                                



echo "Generating diagnostics report for US FIPS Code $FIPS"
~/spew/olympus/reports/make_us_report.sh /mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa/$FIPS


