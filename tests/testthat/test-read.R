context("Read Functions")

test_that("United States functions", {
  library(maptools)
  library(rgeos)
  
  # Make sure we are using the correct data-raw directory 
  # as opposed to the test/testthat one within the package 
  spew_dir <- system.file("", package = "spew")
  data_path <- paste0(spew_dir, "/", "data-raw/10/input")
  
  # Skip if the delaware data is not available!  
  if (!file.exists(data_path)) {
    skip("Skipping: Can't find Delaware Data!")
  }
  
  # Set the new delaware folders
  delaware_folders <- list(pop_table = "counts/natstat/2010/tract", 
                           pums = "pums/natstat/2013/puma", 
                           shapefiles = "shapefiles/natstat/2010/tract", 
                           roads = "roads/natstat/2010/county", 
                           schools = "schools/natstat/2013/county", 
                           lookup = "lookup/natstat/2010/tract", 
                           workplaces = "workplaces/natstat/2009/county", 
                           marginals = "marginals/natstat/2014/tract")

  # Pop Table --------------------------------
  delaware_poptable <- read_pop_table(data_path, 
                                      data_group = "US", 
                                      folders = delaware_folders)
  
  ## Data frame with the correct dimensions 
  expect_equal(nrow(delaware_poptable), 218)
  expect_equal(ncol(delaware_poptable), 4)
  expect_equal(class(delaware_poptable), "data.frame")
  
  ## Making sure stringAsFactors=FALSE 
  expect_equal(any(lapply(delaware_poptable, class) == "factor"), FALSE)
  
  ## Check the Standardization Function
  standard_poptable <- standardize_pop_table(pop_table = delaware_poptable, data_group = "US")
  expect_equal("place_id" %in% names(standard_poptable), TRUE)
  
  ## PUMS -------------------------------
  delaware_pums <- read_pums(data_path, 
                             data_group = "US", 
                             folders = delaware_folders, 
                             vars = list(household = NA, person = NA))
  
  ## Test the rows of households are larger 
  expect_true(names(delaware_pums)[1] == "pums_h")
  expect_true(nrow(delaware_pums$pums_p) > nrow(delaware_pums$pums_h))
  expect_true(nrow(delaware_pums$pums_p) == 8767)
  expect_true(nrow(delaware_pums$pums_h) == 4449)
  
  ## Verify every column is either an integer or a character   
  col_classes <- unlist(c(lapply(delaware_pums$pums_h, class), lapply(delaware_pums$pums_p, class)))
  expect_equal(any(!(col_classes %in% c("character", "integer"))), FALSE)

  ## Verify that the standardized PUMS has the appropriate columns   
  standard_pums <- standardize_pums(delaware_pums, data_group = "US")  
  expect_true("puma_id" %in% names(standard_pums$pums_h))
  expect_true("puma_id" %in% names(standard_pums$pums_p))    
  expect_true("SERIALNO" %in% names(standard_pums$pums_p)) 
  
  # Test that subsetting specific variables works 
  delaware_pums <- read_pums(data_path, 
                             data_group = "US", 
                             folders = delaware_folders, 
                             vars = list(household = c("SERIALNO", "ST"), 
                                         person = c("SERIALNO", "ST")))
  
  expect_equal(ncol(delaware_pums$pums_h), 2)  
  expect_equal(names(delaware_pums$pums_h), c("SERIALNO", "ST"))
  expect_equal(ncol(delaware_pums$pums_h), 2)
  
  # Shapefile --------------------------
  delaware_shape <- read_shapefiles(data_path, 
                                    data_group = "US", 
                                    folders = delaware_folders)
  expect_true(class(delaware_shape$shapefile) == "SpatialPolygonsDataFrame")
  expect_true(class(delaware_shape$roads) == "character")
  
  # Test the reading roads functions, look for speedups   
  roads_path <- paste0(spew_dir, "/data-raw/46/tiger/roads_46")
  sd_roads <- read_roads(path_to_roads = roads_path, road_id = "46111")
  expect_true(class(sd_roads) == "SpatialLinesDataFrame")
  
  # Test the standardization functions
  standard_shape <- standardize_shapefiles(delaware_shape, data_group = "US")
  expect_true(length(standard_shape$shapefile$place_id) == 218)
  expect_true(class(standard_shape$shapefile$place_id) == "character")
  
  # Schools ----------------------------
  delaware_schools <- read_schools(data_path, 
                                   data_group = "US", 
                                   folders = delaware_folders)
  
  expect_true(class(delaware_schools) == "list")
  expect_true(all(names(delaware_schools) == c("public", "private")))
  expect_equal(class(delaware_schools$public$StNo), "character")
  expect_equal(class(delaware_schools$public$CoNo), "character")
  
  # Workplaces --------------------------
  delaware_workplaces <- read_workplaces(data_path, 
                                         data_group = "US", 
                                         folders = delaware_folders)
  expect_true(class(delaware_workplaces) == "data.frame")
  
  # Marginals ---------------------------
  delaware_marginals <- read_marginals(data_path, 
                                       data_group = "US", 
                                       folders = delaware_folders)
  
  expect_true(class(delaware_marginals) == "list")
  expect_true(class(delaware_marginals[[1]]) == "list")
  expect_true(class(delaware_marginals[[1]][[1]]) == "data.frame")
  
  # Overall --------------------------------
  base_path <- file.path(spew_dir, "data-raw/10")
  delaware <- read_data(base_dir = base_path, folders = delaware_folders, data_group = "US", time = FALSE)
  expect_true(all.equal(names(delaware), c("pop_table", "pums", "lookup", "shapefiles",
                                           "schools", "workplaces", "marginals", "moments")))
})



test_that("ipums functions", {
  # Make sure we are using the correct data-raw directory 
  # as opposed to the test/testthat one within the package 
  library(data.table)
  spew_dir <- system.file("", package = "spew")
  data_path <- file.path(spew_dir, "data-raw/ury/input")  
  
  # Skip if the delaware data is not available!  
  if (!file.exists(data_path)) {
    skip("Skipping: Can't find Uruguay Data!")
  }
  
  # Set the folders for the Uruguay data 
  uruguay_folders <- list(pop_table = "counts", 
                          pums = "pums", 
                          shapefiles = "shapefiles")  

  # Pop Table --------------------------------
  uruguay_counts <- read_pop_table(data_path, 
                                   data_group = "ipums", 
                                   folders = uruguay_folders)
  expect_equal(class(uruguay_counts), "data.frame")
  
  standard_counts <- standardize_pop_table(uruguay_counts, data_group = "ipums")
  expect_equal(names(standard_counts), c("place_id", "n_house", "level"))

  # PUMS -------------------------------------
  uruguay_pums <- read_pums(data_path, 
                            data_group = "ipums", 
                            folders = uruguay_folders, 
                            vars = list(household = NA, person = NA))
  expect_equal(class(uruguay_pums), "list")
  expect_equal(names(uruguay_pums), c("pums_h", "pums_p"))
  
  standard_pums <- standardize_pums(pums = uruguay_pums, data_group = "ipums")
  expect_equal("puma_id" %in% names(standard_pums$pums_h), TRUE)

  # Shapefile --------------------------------
  library(maptools)
  uruguay_shape <- read_shapefiles(data_path, 
                                   data_group = "ipums", 
                                   folders = uruguay_folders)
  expect_equal(class(uruguay_shape) == "SpatialPolygonsDataFrame", TRUE)
  expect_equal("place_id" %in% names(uruguay_shape), TRUE)
  
  standard_shape <- standardize_shapefiles(uruguay_shape, data_group = "ipums")
  expect_equal("puma_id" %in% names(standard_shape), TRUE)  

  # Overall ----------------------------------
  base_dir <- file.path(spew_dir, "data-raw/ury")
  uruguay_data <- read_data(base_dir = base_dir, 
                            data_group = "ipums", 
                            folders = uruguay_folders, 
                            time = FALSE)
  
  expect_equal("SERIALNO" %in% names(uruguay_data$pums$pums_p), TRUE)
  expect_equal("puma_id" %in% names(uruguay_data$pums$pums_h), TRUE)
  expect_equal(class(uruguay_data) == "list", TRUE)
})

test_that("custom group read functions", {
  #  Make sure we are using the correct data-raw directory 
  # as opposed to the test/testthat one within the package 
  spew_dir <- system.file("", package = "spew")
  data_path <- paste0(spew_dir, "/", "data-raw/")

  # Skip if the delaware data is not available!  
  if (!file.exists(data_path)) {
    skip("Skipping: Can't find Custom Data!")
  }
  
  custom_folders <- list(pop_table = paste0(data_path, "/ury/input/counts/geohive/2011/1/uruguay_revised.csv"), 
                         pums = list(pums_h = paste0(data_path, "/ury/input/pums/ipums/2011/1/uruguay.csv"), 
                                     pums_p = paste0(data_path, "/ury/input/pums/ipums/2011/1/uruguay.csv")), 
                         shapefiles = paste0(data_path, "/ury/input/shapefiles/ipums/2011/1/uruguay_revised.shp"))
  
  # Counts --------------------------------
  counts <- read_pop_table(input_dir = NULL, 
                           folders = custom_folders, 
                           data_group = "none")
  expect_equal(class(counts), "data.frame")
  expect_error(standardize_pop_table(counts, data_group = "none"), "%in% pt_names is not TRUE", fixed = TRUE)
  
  # Shapefile -----------------------------
  shapefile <- read_shapefiles(input_dir = NULL, 
                               folders = custom_folders,
                               data_group = "none")
  expect_equal(as.character(class(shapefile)), "SpatialPolygonsDataFrame")

  # Pums ---------------------------------
  pums <- read_pums(input_dir = NULL, 
                    folders =custom_folders, 
                    data_group = "none", 
                  vars = list(household = NA, person = NA))
  expect_equal(class(pums), "list")
  expect_equal(class(pums$pums_h), "data.frame")
  expect_error(standardize_pums(pums, data_group = "none"))
})

