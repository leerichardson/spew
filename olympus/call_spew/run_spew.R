# Set the library paths so it checks the personal library first (gets data.table development version)
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

# Parse the command line into SPEW inputs ---
args <- commandArgs(trailingOnly = TRUE)
args <- gsub("\"", "", args)
print(paste0("Arg ", 1:length(args), " ", args))

input_dir <- as.character(args[1])
output_dir <- as.character(args[2])
data_group <- as.character(args[3])
run_type <- as.character(args[4])
if (run_type == "MPI") { library(Rmpi)  } 

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
	if (input_dir == "/mnt/beegfs1/data/shared_group_data/syneco/spew_input/americas/northern_america/usa/72/input") {
	  print("No Schools or Workplaces for Puerto Rico!")
	  
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

} else {
	stop("data_group must be US or ipums")
}

# Print out the parameters of this call to SPEW for the log-file 
print(paste0("Spew Version: ", packageVersion("spew")))
print(paste0("Input Directory: ", input_dir))
print(paste0("Output Directory: ", output_dir))
print(paste0("Data Group: ", data_group))
print(paste0("Run Type: ", run_type))
print(paste0("Sampling Method: ", sampling_method))
print(paste0("Locations Method: ", locations_method))
print(paste0("Folders: ", folders))

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
