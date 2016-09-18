# Lee Richardson, 5/27/2016
# Purpose: Using the lookup table, generate 
# the spew hierarchy directory structure

# Load in the spew hierarchy lookup table and 
# create the base directory 
spew_hierarchy <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/spew_hierarchy/spew_hierarchy.csv", 
                           stringsAsFactors = FALSE)
base <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0"
dir.create(base)

# Loop through each country and recursively create its file-path 
n <- nrow(spew_hierarchy)
for (i in 1:n) {
  # Extract row and it's necessary elements 
  country_row <- spew_hierarchy[i, ]
  region <- country_row[, "region"]
  sub_region <- country_row[, "sub_region"]
  iso3 <- country_row[, "iso3"]
  
  # Generate the file-path and create the directory rcursively 
  filepath <- paste0(base, "/", region, "/", sub_region, "/", iso3)
  dir.create(filepath, recursive = TRUE)
}
