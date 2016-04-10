context("Sampling Locations")

test_that("Sampling functions", {
  
  
  # Load in the South Dakota data 
  library(maptools)
  library(rgeos)
  data(sd_data)
  
  # Verify the Uniform sampling methodology still works 
  number_houses <- 1000
  uniform_locations <- sample_locations(method = "uniform", 
                                        place_id = "46137941600", 
                                        n_house = number_houses, 
                                        shapefile = sd_data$shapefiles)
  
  tract_rds <- readShapeSpatial("/home/lee/spew/data-raw/46/tiger/roads_46/tl_2015_46137_roads.shp")
  roads_shapefile <- list(regions = sd_data$shapefiles, roads = tract_rds)

  road_locs <- sample_locations(method = "roads",                                         
                               place_id = "46137941600", 
                               n_house = number_houses, 
                               shapefile = roads_shapefile, 
                               noise = .01)
  
  # Verify the results are the correct class and equal length 
  expect_true(class(uniform_locations) == "SpatialPoints")
  expect_true(class(uniform_locations) == "SpatialPoints")
  expect_true(length(uniform_locations) == length(road_locs))
})
