context("Sampling Locations")

test_that("Location functions", {

  # Load in the South Dakota data
  library(maptools)
  library(rgeos)
  library(sp)
  data(sd_data)

  # Verify the Uniform sampling methodology still works
  number_houses <- 1000
  pid <- "46135966302"
  sid <- 107
  uniform_locations <- sample_locations(method = "uniform",
                                        place_id = pid,
                                        n_house = number_houses,
                                        shapefile = sd_data$shapefiles, 
                                        noise = .01)
  
  # Set up the roads shapefile
  spew_dir <- system.file("", package = "spew")
  roads_path <- paste0(spew_dir, "/data-raw/46/tiger/roads_46")
  roads_shapefile <- list(regions = sd_data$shapefiles, roads = roads_path)
  
  # Sample from the roads shapefile
  road_locs <- sample_locations(method = "roads",
                               place_id = pid,
                               n_house = number_houses,
                               shapefile = roads_shapefile,
                               noise = .01)

  # Verify uniform sampling works for a large number of points 
  large_num_houses <- 3000000
  uniform_locations_large <- sample_locations(method = "uniform",
                                              place_id = pid,
                                              n_house = large_num_houses,
                                              shapefile = sd_data$shapefiles, 
                                              noise = .001)
  
  # Verify the results are the correct class and equal length
  expect_true(class(uniform_locations) == "SpatialPoints")
  expect_true(class(road_locs) == "SpatialPoints")
  expect_true(length(uniform_locations) == length(road_locs))
  expect_true(length(uniform_locations_large) == large_num_houses)
  
  # Verify sampling from roads works with a small number of houses
  small_number_houses <- 10
  small_road_locs <- sample_locations(method = "roads",
                                place_id = pid,
                                n_house = small_number_houses,
                                shapefile = roads_shapefile,
                                noise = .01)
  expect_true(length(small_road_locs) == small_number_houses)
  
  # Verify the Spatial Points class works for road sampling 
  road_pts <- sp::spsample(roads_shapefile[[1]], n = 100, type = "random")
  road_pts_locs <- samp_roads(100, road_pts, .01)
  expect_true(class(road_pts_locs) == "SpatialPoints")
  
  road_pts2 <- road_pts[1:2, ]
  road_pts_locs2 <- samp_roads(100, road_pts2, .01)
  expect_true(class(road_pts_locs) == "SpatialPoints")
  expect_true(length(road_pts_locs) == length(road_pts_locs2))

  # Test the reading roads functions, look for speedups   
  sd_roads <- read_roads(path_to_roads = roads_path, road_id = "46111")
  expect_true(class(sd_roads) == "SpatialLinesDataFrame")
})
