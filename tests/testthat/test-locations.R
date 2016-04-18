context("Sampling Locations")

test_that("Sampling functions", {
  
  
  # Load in the South Dakota data 
  library(maptools)
  library(rgeos)
  data(sd_data)
  
  # Verify the Uniform sampling methodology still works 
  number_houses <- 1000
  pid <- "46137941600"
  sid <- 107
  uniform_locations <- sample_locations(method = "uniform", 
                                        place_id = pid, 
                                        n_house = number_houses, 
                                        shapefile = sd_data$shapefiles)
  
  tract_rds <- readShapeSpatial("/home/lee/spew/data-raw/46/tiger/roads_46/tl_2015_46137_roads.shp")
  roads_shapefile <- list(regions = sd_data$shapefiles, roads = tract_rds)

  road_locs <- sample_locations(method = "roads",                                         
                               place_id = pid, 
                               n_house = number_houses, 
                               shapefile = roads_shapefile, 
                               noise = .01)
  
  # Verify the results are the correct class and equal length 
  expect_true(class(uniform_locations) == "SpatialPoints")
  expect_true(class(uniform_locations) == "SpatialPoints")
  expect_true(length(uniform_locations) == length(road_locs))
  
  # Plot for the UP-STAT presentation 
#   lake <- sample_locations(method = "uniform", 
#                             place_id = pid, 
#                             n_house = 1, 
#                             shapefile = sd_data$shapefiles)
#     
#   par(mfrow = c(1, 2))
#   
#   plot(sd_data$shapefiles[sid, ], main = "Uniform Sampling")  
#   points(uniform_locations, col = "red", pch = 16, cex = .6)    
#   points(lake, col = "blue", pch = 16, cex = 10)
#   points(uniform_locations, col = "red", pch = 16, cex = .6)    
#   
#   plot(sd_data$shapefiles[sid, ], main = "Road-Based Sampling")  
#   plot(roads_shapefile$roads, add = TRUE)
#   points(road_locs, col = "red", pch = 16, cex = .6)
})
