# Lee Richardson 
# July 7, 2013
# Purpose: Read delaware data from spew_1.2.0 directory structure 
# using the read.R functions, and save this into data/

# Get filepath for delaware input directory
delaware_path <- system.file("extdata/10", package = "spew")

# Set filepaths of subdirectories with each input data type 
delaware_folders <- list(pop_table = "counts/natstat/2010/tract", 
                         pums = "pums/natstat/2013/puma", 
                         shapefiles = "shapefiles/natstat/2010/tract", 
                         roads = "roads/natstat/2010/county", 
                         schools = "schools/natstat/2013/county", 
                         lookup = "lookup/natstat/2010/tract", 
                         workplaces = "workplaces/natstat/2009/county", 
                         marginals = "marginals/natstat/2014/tract")

# Call spews read_data function on the delaware data-directory 
delaware <- read_data(base_dir = delaware_path, folders = delaware_folders, data_group = "US")
delaware <- format_data(data_list = delaware, data_group = "US")

devtools::use_data(delaware, overwrite = TRUE)
