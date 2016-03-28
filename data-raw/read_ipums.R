# Lee Richardson
# 1/2/2016
# Purpose: Example script which converts our raw, 
# uruguay data into a list of .rdata files, which 
# will be used in later (format, make) parts of the 
# program 

uruguay_data <- read_data(input_dir = "data-raw/uruguay", 
                          data_group = "ipums", 
                          folders = list(pop_table = "counts", 
                                         pums = "PUMS", 
                                         shapefiles = "shapefile_ipums"))

# devtools::use_data(uruguay_data, overwrite = TRUE)
library(stringdist)
uruguay_format <- format_data(data_list = uruguay_data, data_group = "ipums")

# devtools::use_data(uruguay_format, overwrite = TRUE)
# write.csv(uruguay_format$pums$pums_h, "data-raw/uruguay/PUMS/pums_hh.csv")

library(parallel)
library(foreach)
library(doSNOW)
data(uruguay_format)
uruguay <- make_data(pop_table = uruguay_format$pop_table, 
           shapefile = uruguay_format$shapefiles, 
           pums_h = uruguay_format$pums$pums_h, 
           pums_p = uruguay_format$pums$pums_p, 
           schools = uruguay_format$schools, 
           workplaces = uruguay_format$workplaces,
           parallel = TRUE, 
           sampling_type = "uniform", 
           convert_count = TRUE, 
           output_dir = "/home/lee/uruguay/")

# Run the full South Dakota data-set 
library(sp)
library(stringdist)
generate_spew(input_dir = "data-raw/uruguay", 
              folders = list(pop_table = "counts", 
                             pums = "PUMS", 
                             shapefiles = "shapefile_ipums"), 
              data_group = "ipums", 
              output_dir = "/home/lee/uruguay/", 
              parallel = TRUE, 
              sampling_type = "uniform", 
              convert_count = FALSE, 
              vars = list(household = NA, person = NA), 
              make_plots = FALSE)

