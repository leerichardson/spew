# Lee Richardson
# 11/17/2015
# Purpose: Example script which converts our raw, 
# south dakota data into a list of .rdata files, which 
# will be used in later (format, make) parts of the program 


# Test the individual read_data functions ----------------------

sd_poptable <- read_pop_table("data-raw/46", data_group = "US", folders = list(pop_table = "popTables", 
                                                                          pums = "pums", 
                                                                          schools = "schools", 
                                                                          lookup = "tables", 
                                                                          shapefiles = "tiger", 
                                                                          workplaces = "workplaces")) 
sd_poptable <- standardize_pop_table(sd_poptable, data_group = "US")



sd_pums <- read_pums("data-raw/46", data_group = "US", folders = list(pop_table = "popTables", 
                                                                               pums = "pums", 
                                                                               schools = "schools", 
                                                                               lookup = "tables", 
                                                                               shapefiles = "tiger", 
                                                                               workplaces = "workplaces")) 
sd_pums <- standardize_pums(sd_pums, data_group = "US")






sd_shape <- read_shapefiles("data-raw/46", data_group = "US", folders = list(pop_table = "popTables", 
                                                                      pums = "pums", 
                                                                      schools = "schools", 
                                                                      lookup = "tables", 
                                                                      shapefiles = "tiger", 
                                                                      workplaces = "workplaces")) 

sd_shape <- standardize_shapefiles(sd_shape, data_group = "US")



  
sd_lookup <- read_lookup("data-raw/46", data_group = "US", folders = list(pop_table = "popTables", 
                                                             pums = "pums", 
                                                             schools = "schools", 
                                                             lookup = "tables", 
                                                             shapefiles = "tiger", 
                                                             workplaces = "workplaces"))
sd_lookup <- standardize_lookup(sd_lookup, data_group = "US")


sd_schools <- read_schools("data-raw/46", data_group = "US", folders = list(pop_table = "popTables", 
                                                                            pums = "pums", 
                                                                            schools = "schools/2013", 
                                                                            lookup = "tables", 
                                                                            shapefiles = "tiger", 
                                                                            workplaces = "workplaces")) 


sd_workplaces <- read_workplaces("data-raw/46", data_group = "US", folders = list(pop_table = "popTables", 
                                                                                  pums = "pums", 
                                                                                  schools = "schools", 
                                                                                  lookup = "tables", 
                                                                                  shapefiles = "tiger", 
                                                                                  workplaces = "workplaces"))

# Use the Read Data function to get the desired list, and save 
# this as an .rdata file in the /data folder for use in debugging 
# the other parts of spew ---------------------------------------
sd_data <- read_data("data-raw/46", data_group = "US", folders = list(pop_table = "popTables", 
                                                                      pums = "pums/2013", 
                                                                      schools = "schools/2013", 
                                                                      lookup = "tables", 
                                                                      shapefiles = "tiger", 
                                                                      workplaces = "workplaces"))

sd_data <- format_data(data_list = sd_data, data_group = "US")
# devtools::use_data(sd_data, overwrite = TRUE)


# Test make_data with the south dakota data ----------------------
data(sd_data)
library(parallel)
library(foreach)
library(doSNOW)
make_data(pop_table = sd_data$pop_table[1:5, ], 
          pums_h = sd_data$pums$pums_h, 
          pums_p = sd_data$pums$pums_p, 
          shapefile = sd_data$shapefiles, 
          schools = sd_data$schools, 
          workplaces = sd_data$workplaces, 
          parallel = TRUE, 
          output_dir = "/home/lee/south_dakota/", 
          sampling_type = "uniform", 
          convert_count = FALSE)
