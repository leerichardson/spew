#!/bin/bash                                                                                          
# PBS Directives ----                                                                                
# Combine stoud and stderr into the same directory
#PBS -j oe                                                                     

# Request 1 node and 64 cores on the node                            
#PBS -l nodes=1:ppn=64                                                                               
# Request a specific amount of time for the job...                   
#PBS -l walltime=12:00:00                                                                            
# Getting data                                                       

ml r/3.2.1
ml pandoc                                

echo "Generating diagnostics report for Uruguay"
~/spew/olympus/reports/make_ipums_report.sh /mnt/beegfs1/data/shared_group_data/syneco/input/$COUNTRYPATH


