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
  
  spew_dir <- system.file("", package = "spew")
  roads_path <- paste0(spew_dir, "/data-raw/46/tiger/roads_46//tl_2015_46137_roads.shp")
  tract_rds <- readShapeSpatial(roads_path)
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
  
  # Verify sampling from roads works with a small number of houses
  small_number_houses <- 10
  small_road_locs <- sample_locations(method = "roads",
                                place_id = pid,
                                n_house = small_number_houses,
                                shapefile = roads_shapefile,
                                noise = .01)
  expect_true(length(small_road_locs) == small_number_houses)
  

#   # Plot for the UP-STAT presentation
#   par(mfrow = c(1, 2))
# 
#   plot(sd_data$shapefiles[sid, ], main = "Uniform Sampling")
#   points(uniform_locations, col = "red", pch = 16, cex = .6)
#   points(-101.8, 44.9, col = "blue", pch = 16, cex = 5.8)
#   points(uniform_locations, col = "red", pch = 16, cex = .6)
# 
#   plot(sd_data$shapefiles[sid, ], main = "Road-Based Sampling")
#   plot(roads_shapefile$roads, add = TRUE)
#   points(road_locs, col = "red", pch = 16, cex = .6)
#   points(-101.8, 44.9, col = "blue", pch = 16, cex = 5.8)
#   
#   # Parallel Plot for UP-STAT
#   par(mfrow = c(1, 3))
#   par(mar=c(0,3,0,1))
#   france1 <- getData('GADM', country = 'FRA', level = 1)
#   france2 <- getData('GADM', country = 'FRA', level = 2)
#   france3 <- getData('GADM', country = 'FRA', level = 3)
#   
#   plot(france1)
#   plot(france2)
#   plot(france3)
  
})
