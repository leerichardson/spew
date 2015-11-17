context("Read Functions")

test_that("Individual United States functions", {
  
  # Make sure we are using the correct data-raw directory 
  # as opposed to the test/testthat one within the package 
  spew_dir <- system.file("", package="spew")
  data_path <- paste0(spew_dir, "/", "data-raw/46")
  
  # Pop Table -------------------------------- 
  sd_poptable <- read_pop_table(data_path, data_group = "US", folders = list(pop_table = "popTables", 
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
  
  
  # PUMS -------------------------------

  
  # Shapefile --------------------------
  
  
  # Schools ----------------------------
  
  
  # Workplace --------------------------
  
  
  # Lookup -----------------------------

  
}) 