context("Location Sampling")

test_that("Single and Multiple Polygons", {
  library(sp)
  library(rgeos)
  data(sd_data)
  
  multiple_polygons <- sample_locations(method = "uniform", 
                                        place_id = 46027965700, n_house = 100, 
                                        shapefile = sd_data$shapefiles$shapefile)
  expect_equal(is.null(multiple_polygons), FALSE)
  
  num_samples <- floor(runif(1, min = 1, max = 200))
  rand_row <- floor(runif(1, min = 1, max = nrow(sd_data$pop_table)))  
  single_polygon <- sample_locations(method = "uniform", 
                                     place_id = sd_data$pop_table[rand_row, "place_id"], 
                                     n_house = num_samples, 
                                     shapefile = sd_data$shapefiles$shapefile)
  expect_equal(length(single_polygon), num_samples)
})

test_that("IPUMS Shapefiles work", {
  library(sp)
  library(rgeos)
  data(uruguay_format)
  
  num_samples <- 100
  place_names <- uruguay_format$shapefiles$place_id
  for (place in place_names[1:6]) {
    samp <- sample_locations(method = "uniform", place, num_samples, uruguay_format$shapefiles)
    expect_equal(length(samp), num_samples)
  }
})

test_that("Uniform, Road, Large and Small", {
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
                                        shapefile = sd_data$shapefiles$shapefile, 
                                        noise = .01)
  
  # Set up the roads shapefile
  spew_dir <- system.file("", package = "spew")
  roads_path <- paste0(spew_dir, "/data-raw/46/tiger/roads_46")
  roads_shapefile <- list(regions = sd_data$shapefiles$shapefile, roads = roads_path)
  
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
                                              shapefile = sd_data$shapefiles$shapefile, 
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
})
