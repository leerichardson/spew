context("Read Functions")

test_that("United States functions", {
  
  # Make sure we are using the correct data-raw directory 
  # as opposed to the test/testthat one within the package 
  spew_dir <- system.file("", package="spew")
  data_path <- paste0(spew_dir, "/", "data-raw/46")
  
  # Pop Table -------------------------------- 
  sd_poptable <- read_pop_table(data_path, 
                                data_group = "US", 
                                folders = list(pop_table = "popTables", 
                                               pums = "pums", 
                                               schools = "schools", 
                                               lookup = "tables", 
                                               shapefiles = "tiger", 
                                               workplaces = "workplaces"))
  
  # Data frame with the correct dimensions 
  expect_equal(nrow(sd_poptable), 222)
  expect_equal(ncol(sd_poptable), 4)
  expect_equal(class(sd_poptable), "data.frame")
  
  # Making sure stringAsFactors=FALSE 
  expect_equal(any(lapply(sd_poptable, class) == "factor"), FALSE)
  
  # Check the Standardization Function
  standard_poptable <- standardize_pop_table(pop_table = sd_poptable, data_group = "US")
  expect_equal("place_id" %in% names(standard_poptable), TRUE)
  
  
  # PUMS -------------------------------
  sd_pums <- read_pums(data_path, data_group = "US", 
                       folders = list(pop_table = "popTables", 
                                      pums = "pums", 
                                      schools = "schools", 
                                      lookup = "tables", 
                                      shapefiles = "tiger", 
                                      workplaces = "workplaces"))
  
  
  # Text the rows of households are larger 
  expect_equal(names(sd_pums)[1] == "pums_h", TRUE)
  expect_equal(nrow(sd_pums$pums_p) > nrow(sd_pums$pums_h), TRUE)
  expect_equal(nrow(sd_pums$pums_p) == 8231 & nrow(sd_pums$pums_h) == 3983, TRUE)
  col_classes <- unlist(c(lapply(sd_pums$pums_h, class), lapply(sd_pums$pums_p, class)))
  expect_equal(any(!(col_classes %in% c("character", "integer"))), FALSE)
  
  standard_pums <- standardize_pums(sd_pums, data_group = "US")  
  expect_equal("puma_id" %in% names(standard_pums$pums_h), TRUE)
  expect_equal("puma_id" %in% names(standard_pums$pums_p), TRUE)    
  expect_equal("SERIALNO" %in% names(standard_pums$pums_p), TRUE) 
  
  # Shapefile --------------------------
  library(maptools)
  sd_shape <- read_shapefiles(data_path, 
                              data_group = "US", 
                              folders = list(pop_table = "popTables", 
                                         pums = "pums", 
                                         schools = "schools", 
                                         lookup = "tables", 
                                         shapefiles = "tiger", 
                                         workplaces = "workplaces")) 
  expect_equal(class(sd_shape) == "SpatialPolygonsDataFrame", TRUE)
  
  # Test the standardization functions
  standard_shape <- standardize_shapefiles(sd_shape, data_group = "US")
  expect_equal(length(standard_shape$place_id) == 222, TRUE)
  expect_equal(class(standard_shape$place_id) == "character", TRUE)
  

  # Lookup -----------------------------

  
  # Schools ----------------------------
  
  
  # Workplace --------------------------
  
  
}) 



test_that("ipums functions", {
  
  # Make sure we are using the correct data-raw directory 
  # as opposed to the test/testthat one within the package 
  spew_dir <- system.file("", package = "spew")
  data_path <- paste0(spew_dir, "/", "data-raw/uruguay")  

  # Pop Table --------------------------------
  uruguay_counts <- read_pop_table(data_path, 
                                data_group = "ipums", 
                                folders = list(pop_table = "counts", 
                                               pums = "PUMS", 
                                               shapefiles = "shapefile"))
  
  expect_equal(class(uruguay_counts), "data.frame")
  
  standard_counts <- standardize_pop_table(uruguay_counts, data_group = "ipums")
  expect_equal(names(standard_counts), c("place_id", "n_house", "level"))

  # PUMS -------------------------------------
  uruguay_pums <- read_pums(data_path, 
                            data_group = "ipums", 
                            folders = list(pop_table = "counts", 
                                           pums = "PUMS", 
                                           shapefiles = "shapefile"))
  
  expect_equal(class(uruguay_pums), "list")
  expect_equal(names(uruguay_pums), c("pums_h", "pums_p"))
  
  standard_pums <- standardize_pums(pums = uruguay_pums, data_group = "ipums")
  expect_equal("puma_id" %in% names(standard_pums$pums_h), TRUE)
  expect_equal("puma_id" %in% names(standard_pums$pums_p), TRUE)
  
  # Shapefile --------------------------------
  library(maptools)
  uruguay_shape <- read_shapefiles(data_path, 
                                   data_group = "ipums", 
                                   folders = list(pop_table = "counts", 
                                                  pums = "PUMS", 
                                                  shapefiles = "shapefile"))
  
  expect_equal(class(uruguay_shape) == "SpatialPolygonsDataFrame", TRUE)
  expect_equal("NAME_1" %in% names(uruguay_shape), TRUE)
  
  standard_shape <- standardize_shapefiles(uruguay_shape, data_group = "ipums")
  expect_equal("place_id" %in% names(standard_shape), TRUE)  

  # Overall ----------------------------------
  uruguay_data <- read_data(data_path, 
                            data_group = "ipums", 
                            folders = list(pop_table = "counts", 
                                           pums = "PUMS", 
                                           shapefiles = "shapefile"))
  
  expect_equal("SERIALNO" %in% names(uruguay_data$pums$pums_p), TRUE)
  expect_equal("puma_id" %in% names(uruguay_data$pums$pums_h), TRUE)
  expect_equal(class(uruguay_data) == "list", TRUE)
})


test_that("no group functions", {
  
  # Make sure we are using the correct data-raw directory 
  # as opposed to the test/testthat one within the package 
  spew_dir <- system.file("", package = "spew")
  data_path <- paste0(spew_dir, "/", "data-raw/")  
  
  # Counts --------------------------------
  
  counts <- read_pop_table(input_dir = NULL, 
                           folders = list(pop_table = paste0(data_path, "uruguay/counts/uruguay_admin.csv"), 
                                          pums = list(pums_h = paste0(data_path, "uruguay/PUMS/858.csv"), 
                                                      pums_p = paste0(data_path, "uruguay/PUMS/858.csv")), 
                                          shapefiles = paste0(data_path, "uruguay/shapefile/URY_adm1.shp")), 
                           data_group = "none")
  
  expect_equal(class(counts), "data.frame")
  expect_error(standardize_pop_table(counts, data_group = "none"), "%in% pt_names is not TRUE", fixed = TRUE)
  
  shapefile <- read_shapefiles(input_dir = NULL, 
                               folders = list(pop_table = paste0(data_path, "uruguay/counts/uruguay_admin.csv"), 
                                              pums = list(pums_h = paste0(data_path, "uruguay/PUMS/858.csv"), 
                                                          pums_p = paste0(data_path, "uruguay/PUMS/858.csv")), 
                                              shapefiles = paste0(data_path, "uruguay/shapefile/URY_adm1.shp")), 
                               data_group = "none")
  
  expect_equal(as.character(class(shapefile)), "SpatialPolygonsDataFrame")
  expect_error(standardize_shapefiles(shapefile, data_group = "none"))
  
  pums <- read_pums(input_dir = NULL, 
                    folders = list(pop_table = paste0(data_path, "uruguay/counts/uruguay_admin.csv"), 
                                   pums = list(pums_h = paste0(data_path, "uruguay/PUMS/858.csv"), 
                                               pums_p = paste0(data_path, "uruguay/PUMS/858.csv")), 
                                   shapefiles = paste0(data_path, "uruguay/shapefile/URY_adm1.shp")), 
                    data_group = "none")
  
  expect_equal(class(pums), "list")
  expect_equal(class(pums$pums_h), "data.frame")
  expect_error(standardize_pums(pums, data_group = "none"))  
  
})

