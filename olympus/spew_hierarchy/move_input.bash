#!/bin/bash

# Create a spew_input file hierarchy, and move spew_1.2.0 data to this location ---
find spew_1.2.0 -type d -name input -print0 | 
	while IFS= read -r -d '' file;  do SRCFILE=$file; DSTFILE=`echo $file | 
	perl -p -i -e 's/spew_1.2.0/spew_input/'`; DSTDIR=`dirname $DSTFILE`; mkdir -p $DSTDIR; mv $SRCFILE $DSTFILE;  done;

