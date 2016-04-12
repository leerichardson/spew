context("SPEW Wrapper")

test_that("SPEW runs as expected", {
  # Load in the data and Libraries 
  library(foreach)
  library(doSNOW)
  library(sp)
  library(maptools)
  library(stringdist)
  
  data(sd_data)
  data(uruguay_format)
  
  # Lee: If you run these, remember to change the output directory!
  # They take too long to put in the automated tests, but they're 
  # a good sanity check to make sure that everything still links up 
  
# Run the full South Dakota data-set 
#   generate_spew(input_dir = "data-raw/46",  
#                 folders = list(pop_table = "popTables", 
#                                pums = "pums/2013", 
#                                schools = "schools/2013", 
#                                lookup = "tables", 
#                                shapefiles = "tiger", 
#                                workplaces = "workplaces"), 
#                 data_group = "US", 
#                 output_dir = "/home/lee/south_dakota/", 
#                 parallel = FALSE, 
#                 sampling_method = "uniform",  
#                 locations_method = "roads",
#                 convert_count = FALSE, 
#                 vars = list(household = NA, person = NA))
  
  # Run the entire uruguay data-set 
#   generate_spew(input_dir = "data-raw/uruguay", 
#                 folders = list(pop_table = "counts", 
#                                pums = "PUMS", 
#                                shapefiles = "shapefile_ipums"), 
#                 data_group = "ipums", 
#                 output_dir = "/home/lee/uruguay/", 
#                 parallel = TRUE, 
#                 sampling_method = "uniform",  
#                 locations_method = "uniform",
#                 convert_count = FALSE, 
#                 vars = list(household = NA, person = NA))
})
