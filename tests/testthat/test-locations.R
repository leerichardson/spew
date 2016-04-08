context("Sampling Locations")

test_that("Sampling functions", {
  
  
  # Load in the South Dakota data 
  data(sd_data)
  number_houses <- 100
  library(maptools)
  library(rgeos)
  
  
  # Verify the Uniform sampling methodology still works 
  uniform_locations <- sample_locations(method = "uniform", 
                                        place_id = "46137941600", 
                                        n_house = number_houses, 
                                        shapefile = sd_data$shapefiles)
  expect_true(class(uniform_locations) == "SpatialPoints")
  
  tract_rds <- readShapeSpatial("/home/lee/spew/data-raw/46/tiger/roads_46/tl_2015_46137_roads.shp")
  roads_shapefile <- list(regions = sd_data$shapefiles, roads = tract_rds)

  road_locs <- sample_locations(method = "roads",                                         
                               place_id = "46137941600", 
                               n_house = number_houses, 
                               shapefile = roads_shapefile, 
                               noise = 0)

  par(mfrow = c(1, 2))
  plot(tract_rds, main = "Sampling from Roads")
  plot(road_locs, add = TRUE, pch = 16, col = "blue")
  plot(uniform_locations, main = "Sampling Uniformly", , pch = 16, col = "blue")

})
