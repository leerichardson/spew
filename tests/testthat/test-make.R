context("Make functions")

test_that("Sampling functions", {
  
  # Load in the formatted data 
  data(sd_data)
  
  # Sample locations --------------
  multiple_polygons <- sample_locations(place_id = 46027965700, n_house = 110, 
                                        shapefile = sd_data$shapefiles)
  expect_equal(is.null(multiple_polygons), FALSE)
  
  num_samples <- floor(runif(1, min = 1, max = 200))
  rand_row <- floor(runif(1, min = 1, max = nrow(sd_data$pop_table)))  
  single_polygon <-sample_locations(place_id = sd_data$pop_table[rand_row, "place_id"], 
                                    n_house = num_samples, shapefile = sd_data$shapefiles)
  expect_equal(length(single_polygon), num_samples)
  
  # Make sure the 0 household places are caught
  test_ind <- 100
  sd_data$pop_table[test_ind, "n_house"] <- 0
  
  expect_output(make_place(test_ind, sd_data$pop_table, sd_data$shapefiles, 
                         sd_data$pums$pums_h, sd_data$pums$pums_p, 
                         sampling_type = "uniform", output_dir = "/home/lee"), 
                          "Place has 0 Households!")
  
  # Test that the Parallel version is quicker ----------------
  library(doParallel)
  library(foreach)
  
  places <- 1:4 
  sink("test_output.txt")
  regular_md <- system.time(make_data(sd_data$pop_table[places, ], sd_data$shapefiles, 
                                      sd_data$pums$pums_h, sd_data$pums$pums_p))
  parallel_md <- system.time(make_data(sd_data$pop_table[places, ], sd_data$shapefiles, 
                                      sd_data$pums$pums_h, sd_data$pums$pums_p, 
                                      parallel = TRUE))
  sink()
  file.remove("test_output.txt")
  
  expect_equal(as.logical(parallel_md[3] < regular_md[3]), TRUE)
}) 
