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
  
  
}) 
