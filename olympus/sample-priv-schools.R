## Script to run private schools

## TODO: load spew instead of just those functions
source("~/spew/R/sample-env-locations.R")

library(maptools)
library(sp)


setwd("/mnt/lustre0/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa/")

states <- list.files()[-length(list.files())]

sapply(states, sample_priv_school_locations)

