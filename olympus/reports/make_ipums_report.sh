#!/bin/bash

folder="$1"
country=$(basename "$folder")

curdir=`pwd`

echo $country

Rscript -e "library(rmarkdown); rmarkdown::render('~/spew/olympus/reports/ipums_diags_beg.Rmd', output_file = '$folder/diags/ipums_diags_$country.html', intermediates_dir = '$folder/diags')" "$folder" "$country"




