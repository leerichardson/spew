# Set the library paths so it checks the personal library first 
# (Used mainly to get the data.table development version)
personal_lib <- grep("leerich", .libPaths())
.libPaths(new = c(.libPaths()[personal_lib]))

# Load SPEW and methods package
library(spew)
library(methods) 

# Spatial packages 
library(sp)
library(maptools)
library(rgeos)

# Speed packages 
library(doParallel)
library(foreach)
library(data.table)
library(bit64)
library(mipfp)

# Data manipulation packages 
library(stringdist)
library(plyr)

# Parse the command line arguments into the inputs for spew 
args <- commandArgs(trailingOnly = TRUE)
args <- gsub("\"", "", args)
print(args)
base_dir <- as.character(args[1])
data_group <- as.character(args[2])

# Set the folders and other specific inputs ---------------
if (data_group == "US") {
  vars <- list(household = c("RT", "TYPE", "SERIALNO", "PUMA", "REGION", "ST", "HINCP", "NP"), 
                  person = c("RT", "SERIALNO", "PUMA", "ST", "SEX", "AGEP",  "SCH", "SCHG", "RELP", 
                             "HISP", "ESR", "PINCP", "NATIVITY", "OCCP", "POBP", "RAC1P"))
  folders <- list(pop_table = "counts/natstat/2010/tract", 
		           pums = "pums/natstat/2013/puma", 
		           shapefiles = "shapefiles/natstat/2010/tract", 
		           roads = "roads/natstat/2010/county", 
		           schools = "schools/natstat/2013/county", 
		           lookup = "lookup/natstat/2010/tract", 
		           workplaces = "workplaces/natstat/2009/county", 
		           marginals = "marginals/natstat/2014/tract")
	sampling_method = "ipf"
	locations_method = "roads"
	parallel = TRUE
	convert_count = TRUE

	# No schools/workplaces for Puerto Rico
	if (base_dir == "/mnt/beegfs1/data/shared_group_data/syneco/input/west/north_america/united_states/72") {
	  folders <- list(pop_table = "counts/natstat/2010/tract", 
	                           pums = "pums/natstat/2013/puma", 
	                           shapefiles = "shapefiles/natstat/2010/tract", 
	                           roads = "roads/natstat/2010/county", 
	                           lookup = "lookup/natstat/2010/tract", 
	                           marginals = "marginals/natstat/2014/tract")
	}	
} 

# Print out the parameters of this call to SPEW for the log-file 
print(paste0("Spew Version: ", packageVersion("spew")))
print(paste0("Directory: ", base_dir))
print(paste0("Data Group: ", data_group))
print(paste0("Parallel: ", parallel))
print(paste0("Sampling People Method: ", sampling_method))
print(paste0("Sampling Locations Method: ", locations_method))

# Call the SPEW wrapper function 
call_spew(base_dir = base_dir, 
	        folders = folders, 
	        data_group = data_group, 
	        parallel = parallel, 
	        sampling_method = sampling_method, 
	        locations_method = locations_method, 
	        convert_count = convert_count, 
	        vars = vars)
