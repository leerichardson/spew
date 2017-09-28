# Set the library paths so it checks the personal library first (gets data.table development version)
#personal_lib <- "/mnt/beegfs1/users/leerich/R/x86_64-pc-linux-gnu-library/3.3"
library(devtools)
devtools::load_all("/mnt/beegfs1/data/shared_group_data/syneco/spew")

library(rgdal)
library(methods) 
library(sp)
library(maptools)
library(rgeos)
library(foreach)
library(doParallel)
library(parallel)
library(data.table)
library(mipfp)
library(stringdist)
library(plyr)
library(Rmpi)

# Parse the command line into SPEW inputs ---
args <- commandArgs(trailingOnly = TRUE)
args <- gsub("\"", "", args)
print(paste0("Arg ", 1:length(args), " ", args))

input_dir <- as.character(args[1])
output_dir <- as.character(args[2])
data_group <- as.character(args[3])
run_type <- as.character(args[4])

# Set the folders and other specific inputs ---
if (data_group == "US") {
	folders <- list(pop_table = "counts/natstat/2010/tract", 
		           pums = "pums/natstat/2013/puma", 
		           shapefiles = "shapefiles/natstat/2010/tract", 
		           roads = "roads/natstat/2010/county", 
		           schools = "schools/natstat/2013/county", 
		           lookup = "lookup/natstat/2010/tract", 
		           workplaces = "workplaces/natstat/2009/county", 
		           marginals = "marginals/natstat/2014/tract")
	vars <- list(household = c("SERIALNO", "PUMA", "NP", "HINCP"), 
		         person = c("SERIALNO", "PUMA", "SEX", "AGEP", "RAC1P", "SCH", "SCHG", "ESR", "RELP"))
	sampling_method <- "ipf"
	locations_method <- "roads"
	convert_count <- FALSE

	# No schools/workplaces for Puerto Rico
	if (input_dir == "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa/72") {
	  folders <- list(pop_table = "counts/natstat/2010/tract", 
                       pums = "pums/natstat/2013/puma", 
                       shapefiles = "shapefiles/natstat/2010/tract", 
                       roads = "roads/natstat/2010/county", 
                       lookup = "lookup/natstat/2010/tract", 
                       marginals = "marginals/natstat/2014/tract")
	}

} else if (data_group == "ipums") {	
	folders <- list(pop_table = "counts", 
	              pums = "pums", 
	              shapefiles = "shapefiles")

	vars = list(household = c("SERIAL", "COUNTRY","YEAR","PERSONS","GEOLEV1", "HHTYPE","PERNUM"), 
				person = c("SERIAL","AGE","SEX","RACE","SCHOOL","INCTOT"))

	sampling_method <- "uniform"
	locations_method <- "uniform"
	convert_count <- TRUE
	run_type = "MC"

} else if (data_group == "none") {
	# Set the custom file-paths for Canada!
	if (basename(input_dir) == "can") {
		folders <- list(pop_table = file.path(input_dir, "input/counts/natstat/2011/4/pop_table.csv"), 
                 pums = list(pums_h = file.path(input_dir, "input/pums/natstat/2011/4/pums_h.csv"),  
                 			 pums_p = file.path(input_dir, "input/pums/natstat/2011/4/pums_p.csv")),
                 shapefiles = file.path(input_dir, "input/shapefiles/natstat/2011/4/canada_shapefiles.shp"))

		# Set the specific variables for Canada 
		vars = list(household = c("SERIALNO", "puma_id"), 
					person = c("SERIALNO", "AGEGRP","HRSWRK","IMMSTAT",
						"INCTAX","MODE","OCC","POB","RELIGION","SEX"))
			sampling_method <- "uniform"
		locations_method <- "uniform"
		convert_count <- TRUE
	}
	parallel_type = "SOCK"

}

# Print out the parameters of this call to SPEW for the log-file 
print(paste0("Spew Version: ", packageVersion("spew")))
print(paste0("Input Directory: ", input_dir))
print(paste0("Output Directory: ", output_dir))
print(paste0("Data Group: ", data_group))
print(paste0("Run Type: ", run_type))
print(paste0("Sampling Method: ", sampling_method))
print(paste0("Locations Method: ", locations_method))

# Call the SPEW wrapper function 
call_spew(input_dir = input_dir, 
		  output_dir = output_dir, 
		  folders = folders, 
		  vars = vars, 
		  data_group = data_group,
		  run_type = run_type, 
		  sampling_method = sampling_method, 
		  locations_method = locations_method,
		  convert_count = convert_count)

