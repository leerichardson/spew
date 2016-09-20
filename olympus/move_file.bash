#!/bin/bash

# Read in the file as input and move it onto our Olympus directory
file=$1
scp -r /home/lee/Dropbox/spew/olympus/$file leerich@login.olympus.psc.edu:/mnt/beegfs1/data/shared_group_data/syneco/olympus/$file
