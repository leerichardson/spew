#!/bin/bash

folder="$1"
country=${folder##*/}
country=${country%.*}

curdir=`pwd`

echo "$curdir"

Rscript -e "library(rmarkdown); rmarkdown::render('~/spew/olympus/reports/us_diags.Rmd', output_file = 'reports/$country/us_diags_$country.html')" "$folder"
