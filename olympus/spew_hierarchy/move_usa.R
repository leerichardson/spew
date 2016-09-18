# Lee Richardson 
# September 10, 2016
# Purpose: Move USA data to the new, spew_1.2.0 hierarchy

base_dir <- "/mnt/beegfs1/data/shared_group_data/syneco/input/west/north_america/united_states"
new_dir <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa"
usa_lookup <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/spew_hierarchy/country_hierarchies/usa_lookup.csv", stringsAsFactors = FALSE)

n <- nrow(usa_lookup)
for (i in 1:n) {
	# Extract the state ID and make sure it matches 
	state_row <- usa_lookup[i, ]
	id <- as.character(state_row[, 1])
	if (nchar(id) == 1) {
		id <- paste0("0", id)
	}
	print(paste0("State: ", i))
	
	# Set old directory and create the new one 
	old_dir <- paste0(base_dir, "/", id, "/input/")
	new_state_dir <- paste0(new_dir, "/", id, "/input")
	dir.create(new_state_dir, recursive = TRUE) 
	
	# Move regular shapefiles 	
	shapefiles <- list.files(paste0(old_dir, "tiger"))
	shapes <- grep("2010", shapefiles)
	old_shapefile <- paste0(old_dir, "tiger/", shapefiles[shapes])
	new_shapefile <- paste0(new_state_dir, "/shapefiles/natstat/2010/tract")
	dir.create(new_shapefile, recursive = TRUE)
	shapefile_list <- paste0(old_shapefile, "/", list.files(old_shapefile))
	file.copy(shapefile_list, new_shapefile, overwrite = FALSE)

	# Move road files 
	roads <- grep("roads", shapefiles)
	old_roads <- paste0(old_dir, "tiger/", shapefiles[roads])
	new_roads <- paste0(new_state_dir, "/roads/natstat/2010/county")
	dir.create(new_roads, recursive = TRUE)
	roads_list <- paste0(old_roads, "/", list.files(old_roads))
	file.copy(from = roads_list, new_roads, overwrite = FALSE)

	# Move pop-tables
	pop_tables <- list.files(paste0(old_dir, "popTables"), full.names = TRUE)
	new_counts <- paste0(new_state_dir, "/counts/natstat/2010/tract")
	dir.create(new_counts, recursive = TRUE)
	file.copy(pop_tables, new_counts, overwrite = FALSE)

	# Move ACS-PUMS 
	pums_2011 <- list.files(paste0(old_dir, "pums"), recursive = FALSE, full.names = TRUE)
	pums_2011_files <- grep(".csv", pums_2011)
	pums_2011 <- pums_2011[pums_2011_files]	 
	pums_2013 <- list.files(paste0(old_dir, "pums/2013"), full.names = TRUE)
	
	new_pums_2011 <- paste0(new_state_dir, "/pums/natstat/2011/puma")
	new_pums_2013 <- paste0(new_state_dir, "/pums/natstat/2013/puma")
	dir.create(new_pums_2011, recursive = TRUE)
	dir.create(new_pums_2013, recursive = TRUE)

	file.copy(pums_2011, new_pums_2011, overwrite = FALSE)
	file.copy(pums_2013, new_pums_2013, overwrite = FALSE)

	# Move schools 
	schools_2011 <- list.files(paste0(old_dir, "schools/2011"), recursive = FALSE, full.names = TRUE)
	schools_2013 <- list.files(paste0(old_dir, "schools/2013"), recursive = FALSE, full.names = TRUE)
	new_schools_2011 <- paste0(new_state_dir, "/schools/natstat/2011/county")
	new_schools_2013 <- paste0(new_state_dir, "/schools/natstat/2013/county")
	dir.create(new_schools_2011, recursive = TRUE)
	dir.create(new_schools_2013, recursive = TRUE)
	file.copy(schools_2011, new_schools_2011, overwrite = FALSE)
	file.copy(schools_2013, new_schools_2013, overwrite = FALSE)

	# Move workplaces 
	workplaces <- list.files(paste0(old_dir, "workplaces"), recursive = FALSE, full.names = TRUE)
	new_workplaces <- paste0(new_state_dir, "/workplaces/natstat/2009/county")
	dir.create(new_workplaces, recursive = TRUE)
	file.copy(workplaces, new_workplaces, overwrite = FALSE)	

	# Move tables 
	lookups <- list.files(paste0(old_dir, "tables"), recursive = FALSE, full.names = TRUE)
	new_lookups <- paste0(new_state_dir, "/lookup/natstat/2010/tract")
	dir.create(new_lookups, recursive = TRUE)
	file.copy(lookups, new_lookups, overwrite = TRUE)
}

