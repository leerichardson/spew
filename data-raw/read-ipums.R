# Lee Richardson
# 1/2/2016
# Purpose: Example script which converts our raw, 
# uruguay data into a list of .rdata files, which 
# will be used in later (format, make) parts of the 
# program 

# Updated: July 7, 2017
uruguay_path <- system.file("extdata/ury", package = "spew")

uruguay <- read_data(base_dir = uruguay_path, 
                          data_group = "ipums", 
                          folders = list(pop_table = "counts", 
                                         pums = "pums", 
                                         shapefiles = "shapefiles"))

uruguay <- format_data(data_list = uruguay, data_group = "ipums")

devtools::use_data(uruguay, overwrite = TRUE)
