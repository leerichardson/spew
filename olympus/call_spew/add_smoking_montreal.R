## Script to add smoking and network status to current montreal synthetic ecosystems
## Updated 10/24/2016


## library(spew)
library(devtools)
library(plyr)
load_all()
path <- "~/Dropbox/Montreal"
synth_pop_fn <- file.path(path, "spew_montreal/people_4620700.01.csv")
output_path <- "~/Desktop/"
pop_type <- "b"
char_pums <- read.csv(file.path(path, "MoNNET Network Data FSA", "MoNNET-HA_for Pittsburgh Supercomputing Center_20160331.csv"))
## marginals
var_name <- "AGEGRP"
type <- "ord"
bounds <- data.frame(lower = c(0, 9), upper=c(8, Inf))
category_name <- c("AGEGRP-0-24", "AGEGRP-25+")
marginals <- spew:::make_cat_var_obj(var_name, type, bounds, category_name)
char_pums$AGEGRP <- "AGEGRP-9-Inf"
##
pop_type <- "both"
args <- list(suffix = "smoking", char_pums = char_pums, marginals = marginals, output_path = output_path)
synth_pop_path <- file.path(path, "spew_montreal")


out <- spew:::add_characteristic(synth_pop_path, args, pop_type)
