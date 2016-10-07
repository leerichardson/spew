#!/bin/bash

folder="$1"
country=${folder##*/}
country=${country%.*}

curdir=`pwd`

echo "$curdir"


Rscript -e "library(rmarkdown); rmarkdown::render('~/spew/olympus/reports/ipums_diags.Rmd', output_file = 'reports/ipums_diags_$country.html')" "$folder"
