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
                                         shapefiles = "shapefile"))

# devtools::use_data(uruguay_data, overwrite = TRUE)
library(stringdist)
uruguay_format <- format_data(data_list = uruguay_data, data_group = "ipums")

# devtools::use_data(uruguay_format, overwrite = TRUE)
# write.csv(uruguay_format$pums$pums_h, "data-raw/uruguay/PUMS/pums_hh.csv")

# library(parallel)
# library(foreach)
make_data(pop_table = uruguay_format$pop_table, 
           shapefile = uruguay_format$shapefiles, 
           pums_h = uruguay_format$pums$pums_h, 
           pums_p = uruguay_format$pums$pums_p, 
           parallel = FALSE, 
           sampling_type = "uniform", 
           convert_count = TRUE, 
           output_dir = "/home/lee/uruguay/")


