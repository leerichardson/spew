#!/bin/bash

folder="$1"
tract="$2"
fips="$3"

curdir=`pwd`

echo "$curdir"

Rscript -e "library(rmarkdown); rmarkdown::render('~/spew/olympus/reports/us_diags_tracts.Rmd', output_file = '~/tracts/$fips/$tract/us_diags_$tract.html')" "$folder" "$tract"
