# Lee Richardson
# 1/22/2016
# Purpose: This file is meant to prepare the 
# ipums shapefiles and counts for use by 
# spew on Olympus

# Prepare the data and R Session --------------------------

# Clear R's workspace and load in the functions 
# needed to read and format the data
rm(list = ls())
library(spew)
library(stringdist)
library(maptools)

# Read in the shapefile and counts 
country <- ""
path <- ""

in_dir <- paste0("/data/shared_group_data/syneco/input/", path)
out_pt <- paste0("/data/shared_group_data/syneco/input/", 
				path, "/counts/", country, "_revised.csv")
out_shape <- paste0("/data/shared_group_data/syneco/input/", 
					path, "/shapefile_ipums/", country, "_revised")

data_group <- "ipums"
folders <- list(pop_table = "counts", 
                    pums = "PUMS", 
                    shapefiles = "shapefile_ipums")

pop_table <- spew:::read_pop_table(in_dir, folders, data_group)
pop_table <- spew:::standardize_pop_table(pop_table, data_group)

shapefile <- spew:::read_shapefiles(in_dir, folders, data_group)
shapefile <- spew:::standardize_shapefiles(shapefile, data_group)

# Obtain the shapefile and count names
shapefile_names <- shapefile$place_id
level <- spew:::get_level(shapefile_names, pop_table)
count_indices <- which(pop_table$level == level)
count_names <- pop_table$place_id[count_indices]

# Clean the names of their non ascii character, 
# uppercases, and spaces 
count_names <- spew:::clean_names(count_names)
shapefile_names <- spew:::clean_names(shapefile_names)


# ----------------------------------------------
# MAKE CHANGES HERE ----------------------------
# ----------------------------------------------




# Verify the changes work and write 
# the edited files  ------------------------------------
shapefile_indices <- amatch(count_names, shapefile_names, 
							method = "jw", maxDist = .3)

stopifnot(!any(is.na(shapefile_indices)))
stopifnot(!any(duplicated(shapefile_indices)))

# Write the "revised" version of the 
# pop_table AND shapefile if necessary 
pop_table[count_indices, "place_id"] <- shapefile_names[shapefile_indices]
write.table(pop_table, 
 			file = out_pt, 
 			sep = ",", 
 			row.names = FALSE, 
 			qmethod = "double")

shapefile$place_id <- shapefile_names
writeSpatialShape(shapefile, out_shape)