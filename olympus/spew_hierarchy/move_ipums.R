# Lee Richardson, 6/1/2016
# Purpose: Move input data into new spew hierarchy 

# Clear R's workspace 
rm(list = ls())
options(error = recover)

# Read in the old and new lookup tables 
new_lookup <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/spew_hierarchy/spew_hierarchy.csv", stringsAsFactors = FALSE)
old_lookup <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/input/spew_lookup.csv", stringsAsFactors = FALSE)

# Read in the data-source lookup tables 
geohive_lookup <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/getting_data/geohive/geohive_lookup.csv", stringsAsFactors = FALSE) 
gadm_lookup <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/getting_data/gadm/gadm_lookup.csv", stringsAsFactors = FALSE)
ipums_lookup <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/getting_data/ipums/ipums_lookup.csv", stringsAsFactors = FALSE)

# Algorithm to move data into the new hierarchy -----------------
num_countries <- nrow(new_lookup)
new_base_dir <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0"
old_base_dir <- "/mnt/beegfs1/data/shared_group_data/syneco/input"

# Set the base paths for the input data 
gadm_base_path <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/getting_data/gadm/gadm_files"
ipums_pums_path <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/getting_data/ipums/countries"
ipums_shp_path <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/getting_data/ipums/world_geolev1"
geohive_path <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/getting_data/geohive" 

# Loop through each country in the new hierarchy and 
# move all of the relevant data into its new directory 
for (i in 1:num_countries) {
	# Construct the filepath for country in new hierarchy 
	iso3 <- new_lookup[i, "iso3"]
	region <- new_lookup[i, "region"]
	sub_region <- new_lookup[i, "sub_region"]
	new_path <- paste0(new_base_dir, "/", region, "/", sub_region, "/", iso3)
	print(new_path)	

	# Clear out any of the new data that's already in there 
	old_inputs <- paste0(new_path, "/input")
	unlink(old_inputs, recursive = TRUE)

	# Skip Canada for now 
	if (iso3 == "can") {
		print("Skipping Canada for now")
		next
	}

	# Construct a filepath to the old hierarchy if the 
	# iso3 exists in the old lookup table
	row <- which(old_lookup$iso3 == iso3)
	if (length(row) == 0) {
	} else {
		hemisphere <- old_lookup[row, "hemisphere"]
		continent <- old_lookup[row, "continent"]
		country <- old_lookup[row, "country"]
		old_path <- paste0(old_base_dir, "/", hemisphere, "/", continent, "/", country)
		old_files <- list.files(paste0(old_path, "/input"))
	}
	
	# GADM --------------
	
	# Check to see if the gadm file for this country exists. 
	# If yes, copy the zip file into the new directroy and unzip  
	gadm_row <- which(gadm_lookup$iso3 == iso3)
	if (length(gadm_row) == 1) {
		print("Moving GADM Files")
		# Extract the filepath of the GADM file 	
		gadm_abbr <- gadm_lookup[gadm_row, "abbreviation"]
		gadm_filepath <- paste0(gadm_base_path, "/", gadm_abbr, ".zip")		
		stopifnot(file.exists(gadm_filepath))		

		# Create the directory where the GADM file will be unzipped 
		gadm_year <- gadm_lookup[gadm_row, "year"]
		gadm_level <- gadm_lookup[gadm_row, "level"]
		new_gadm_path <- paste0(new_path, "/input/shapefiles/gadm/", gadm_year, "/", gadm_level)
		dir.create(new_gadm_path, recursive = TRUE)
		unzip(gadm_filepath, exdir = new_gadm_path)
	}

	# IPUMS -------------
	
	# Copy over the new pums data and move over 
	# the existing shapefile if there. If not, 
	# copy over the shapefile data from lookup table 
	ipums_row <- which(ipums_lookup$iso3 == iso3)
	if (length(ipums_row) == 1 & iso3 != "npl") {
		print("Moving IPUMS Microdata")
		# Extract path to the data-source 
		microdata_name <- ipums_lookup[ipums_row, "ipums_filenames"]
		md_path <- paste0(ipums_pums_path, "/", microdata_name)
		stopifnot(file.exists(md_path))		

		# Create new directory to move data-source 
		ipums_year <- ipums_lookup[ipums_row, "years"]
		ipums_level <- ipums_lookup[ipums_row, "level"]
		new_md_path <- paste0(new_path, "/input/pums/ipums/", ipums_year, "/", ipums_level)
		dir.create(new_md_path, recursive = TRUE)
		copy_data <- file.copy(from = md_path, to = new_md_path)
		stopifnot(copy_data)

		# Search for the old shapefile. If there, copy 
		# it into the new directory. If not, copy it over 
		# from the ipums lookup table 
                new_ipm_shp_path <- paste0(new_path, "/input/shapefiles/ipums/", ipums_year, "/", ipums_level)
		old_ipums_shp <- paste0(old_path, "/input/shapefile_ipums")
		if (dir.exists(old_ipums_shp)) {
			print("MOVING IPUMS SHAPEFILES")
			ipums_shp_files <- list.files(old_ipums_shp, full.names = TRUE)
			dir.create(new_ipm_shp_path, recursive = TRUE)
			file.copy(ipums_shp_files, new_ipm_shp_path)
		} else {
			print("NEW IPUMS SHAPEFILES")
			if (iso3 == "lca") {
				print("No Santa Lucia Shapefile!")
			} else {
				# Obtain all shapefiles from original source 
				# and move these into the new lookup directory
				shapefile_country_name <- ipums_lookup[ipums_row, "country"]
				all_ipums_shp <- list.files(ipums_shp_path, full.names = TRUE)
				ipums_oldshp_files <- all_ipums_shp[grep(shapefile_country_name, all_ipums_shp)]
				dir.create(new_ipm_shp_path, recursive = TRUE)
				file.copy(ipums_oldshp_files, new_ipm_shp_path)					
			}
		} 		
	}	

	# GEOHIVE -----------
	geohive_row <- which(geohive_lookup$iso3 == iso3)
 	geo_years <- geohive_lookup[geohive_row, "year"]
	geo_levels <- geohive_lookup[geohive_row, "level"]
	new_geohive_path <- paste0(new_path, "/input/counts/geohive/", geo_years, "/", geo_levels)	
	if (exists("old_path")) {
		print("Old geohive counts")
		old_geohive_path <- paste0(old_path, "/input/counts")
		old_geo_files <- list.files(old_geohive_path, full.names = TRUE)
		
		# Add in years and levels if there's a revised 
		# file. Set this as level 1 for compatibility with 
		# the ipums shapefiles they were merged with 
		revised_check <- grep("revised", old_geo_files)
		if (length(revised_check) != 0) {
			stopifnot(revised_check == length(old_geo_files))
		}
		if (length(revised_check) == 1) {
		
			revised_path <- paste0(new_path, "/input/counts/geohive/", geo_years[1], "/1")	
			new_geohive_path <- c(new_geohive_path, revised_path)
		}
		for (p in seq_along(new_geohive_path)) {
			dir.create(new_geohive_path[p], recursive = TRUE)
			file.copy(old_geo_files[p], new_geohive_path[p])
		}		
	
	} else if (length(geohive_row) == 0) {
		print("No geohive counts")
	} else {
		print("New geohive counts")
		country <- geohive_lookup[geohive_row, "country"]
		filetypes <- geohive_lookup[geohive_row, "filetype"]
		geo_filepaths <- paste0(geohive_path, "/", country, "/", country, "_", filetypes, ".csv")
		for (p in seq_along(new_geohive_path)) {
			dir.create(new_geohive_path[p], recursive = TRUE)
			file.copy(geo_filepaths[p], new_geohive_path[p])
		}
	}
}


