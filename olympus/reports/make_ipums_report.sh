#!/bin/bash

folder="$1"
country=$(basename "$folder")

curdir=`pwd`

echo $country

Rscript -e "library(rmarkdown); rmarkdown::render('~/spew/olympus/reports/ipums_diags.Rmd', output_file = '~/ipums_reports/ipums_diags_$country.html')" "$folder"
