#!/bin/bash

folder="$1"
country=${folder##*/}
country=${country%.*}

curdir=`pwd`

echo "$curdir"

Rscript -e "library(rmarkdown);library('spew', lib.loc=.libPaths()[2]); us = readRDS('~/spew/olympus/reports/us.rds'); fps = '$country'; state_name <<- spew:::fipsToPlaceName(fps, level = 'state', df = us); toupper(fps); rmarkdown::render('~/spew/olympus/reports/us_diags_beg.Rmd', output_file = '$folder/diags/us_diags_$country.html', intermediates_dir = '$folder/diags')" "$folder"
