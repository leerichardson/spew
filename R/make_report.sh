#!/bin/bash

folder="$1"

curdir=pwd

echo "$curdir"

Rscript -e "library(rmarkdown); rmarkdown::render('report.Rmd')" "$folder"
