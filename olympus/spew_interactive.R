options(error = recover)

# ---------- Set the data-group and filepath ------------------------------
data_group <- "US"
spew_dir <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0"
region <- "americas"
subregion <- "northern_america"
iso3 <- "usa/10"
base_dir <- file.path(spew_dir, region, subregion, iso3)
# -------------------------------------------------------------------------

# Set the library paths so it checks the personal library first 
# (Used mainly to get the data.table development version)
personal_lib <- grep("leerich", .libPaths())
.libPaths(new = c(.libPaths()[personal_lib]))

# Load SPEW and methods package
library(spew)
library(methods) 
library(Rcpp)

# Spatial packages 
library(sp)
library(maptools)
library(rgeos)

# Parallel Packages 
library(doMC)
library(Rmpi)
library(doSNOW)
library(foreach)

# Speed packages 
library(data.table)
library(bit64)

# IPF
library(mipfp)

# Data manipulation packages 
library(stringdist)
library(plyr)

# Set the folders and other specific inputs ---------------
if (data_group == "US") {
  vars <- list(household = c("RT", "TYPE", "SERIALNO", "PUMA", "HINCP", "NP"), 
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
	sampling_method <- "ipf"
	locations_method <- "roads"
	convert_count <- FALSE
	parallel_type = "MPI"

	# No schools/workplaces for Puerto Rico
	if (base_dir == "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa/72") {
	  folders <- list(pop_table = "counts/natstat/2010/tract", 
                       pums = "pums/natstat/2013/puma", 
                       shapefiles = "shapefiles/natstat/2010/tract", 
                       roads = "roads/natstat/2010/county", 
                       lookup = "lookup/natstat/2010/tract", 
                       marginals = "marginals/natstat/2014/tract")
	}

} else if (data_group == "ipums") {	
	print("Ipums!")
	folders <- list(pop_table = "counts", 
	                  pums = "pums", 
	                  shapefiles = "shapefiles")
	vars = list(household = c("COUNTRY","YEAR","SERIAL","PERSONS","GEOLEV1",
								"HHTYPE","PERNUM"), 
				person = c("SERIAL","AGE","SEX","RACE","SCHOOL","INCTOT"))
	sampling_method <- "uniform"
	locations_method <- "uniform"
	parallel <- TRUE
	convert_count <- TRUE
	parallel_type = "SOCK"

} else if (data_group == "none") {
	# Set the custom file-paths for Canada!
	if (basename(base_dir) == "can") {
		folders <- list(pop_table = file.path(base_dir, "input/counts/natstat/2011/4/pop_table.csv"), 
                 pums = list(pums_h = file.path(base_dir, "input/pums/natstat/2011/4/pums_h.csv"),  
                 			 pums_p = file.path(base_dir, "input/pums/natstat/2011/4/pums_p.csv")),
                 shapefiles = file.path(base_dir, "input/shapefiles/natstat/2011/4/canada_shapefiles.shp"))

		# Set the specific variables for Canada 
		vars = list(household = c("SERIALNO", "puma_id"), 
					person = c("SERIALNO", "AGEGRP","HRSWRK","IMMSTAT",
						"INCTAX","MODE","OCC","POB","RELIGION","SEX"))
			sampling_method <- "uniform"
		locations_method <- "uniform"
		parallel <- TRUE
		convert_count <- TRUE
		parallel_type = "MPI"
	}
}

# Print out the parameters of this call to SPEW for the log-file 
print(paste0("Spew Version: ", packageVersion("spew")))
print(paste0("Directory: ", base_dir))
print(paste0("Data Group: ", data_group))
print(paste0("Parallel Backend: ", parallel_type))
print(paste0("Sampling People Method: ", sampling_method))
print(paste0("Sampling Locations Method: ", locations_method))

# Call the SPEW wrapper function 
call_spew(base_dir = base_dir, 
	        folders = folders, 
	        data_group = data_group, 
	        sampling_method = sampling_method, 
	        locations_method = locations_method, 
	        convert_count = convert_count, 
	        vars = vars, 
	        parallel_type = parallel_type)

