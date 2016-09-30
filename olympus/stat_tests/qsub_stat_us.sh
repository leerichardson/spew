#PBS -l walltime=24:00:00   #!/bin/bash                                                                                          
# PBS Directives ----                                                                                
# Combine stoud and stderr into the same directory                   #PBS -j oe                                                                     

# Request 1 node and 64 cores on the node                            #PBS -l nodes=1:ppn=64                                                                               
# Request a specific amount of time for the job...                   #PBS -l walltime=5:00:00                                                                            
# Getting data                                                       

ml r/3.2.1
ml pandoc                                



echo "Performing chi-square tests on state"
~/spew/olympus/stat_tests/test_state.R /mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern/northern_america/usa/$FIPS $summary_dir $alpha


