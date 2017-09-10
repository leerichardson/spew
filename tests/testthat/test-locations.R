context("Location Sampling")

test_that("Single and Multiple Polygons", {
  library(sp)
  data(delaware)
  row <- delaware$pop_table[100, ]
  
  
  multiple_polygons <- sample_locations(method = "uniform", 
                                        place_id = row[, "place_id"], n_house = row[, "n_house"], 
                                        shapefile = delaware$shapefiles$shapefile)
  expect_equal(is.null(multiple_polygons), FALSE)
  
  num_samples <- floor(runif(1, min = 1, max = 200))
  rand_row <- floor(runif(1, min = 1, max = nrow(delaware$pop_table)))  
  single_polygon <- sample_locations(method = "uniform", 
                                     place_id = delaware$pop_table[rand_row, "place_id"], 
                                     n_house = num_samples, 
                                     shapefile = delaware$shapefiles$shapefile)
  expect_equal(length(single_polygon), num_samples)
})

test_that("IPUMS Shapefiles work", {
  library(sp)
  data(uruguay)
  
  num_samples <- 100
  place_names <- uruguay$shapefiles$place_id
  for (place in place_names[1:6]) {
    samp <- sample_locations(method = "uniform", place, num_samples, uruguay$shapefiles)
    expect_equal(length(samp), num_samples)
  }
  
})


test_that("Road sampling works", {
  skip_if_not_installed("rgeos")
  library(rgeos)
  library(sp)
  library(maptools)

  data(delaware)
  row <- 2
  pid <- delaware$pop_table$place_id[row]
  number_houses <- delaware$pop_table$n_house[row]
  
  data_path <- system.file("extdata/10/input", package = "spew")
  roads_path <- paste0(data_path, "/roads/natstat/2010/county")
  roads_shapefile <- list(regions = delaware$shapefiles$shapefile, roads = roads_path)
  
  # Sample from the roads shapefile
  road_locs <- sample_locations(method = "roads",
                                place_id = pid,
                                n_house = number_houses,
                                shapefile = roads_shapefile,
                                noise = .01)
  # Sample Uniformly 
  uniform_locs <- sample_locations(method = "uniform",
                                        place_id = pid,
                                        n_house = number_houses,
                                        shapefile = delaware$shapefiles$shapefile, 
                                        noise = .01)
  
  expect_true(length(uniform_locs) == length(road_locs))
  
  
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

test_that("Uniform Large Households", {
  # Load in the South Dakota data
  library(maptools)
  library(sp)
  data(delaware)

  # Verify the Uniform sampling methodology still works
  number_houses <- 1000
  pid <- delaware$pop_table[100, "place_id"]

  uniform_locations <- sample_locations(method = "uniform",
                                        place_id = pid,
                                        n_house = number_houses,
                                        shapefile = delaware$shapefiles$shapefile, 
                                        noise = .01)

  # Verify uniform sampling works for a large number of points 
  large_num_houses <- 3000000
  uniform_locations_large <- sample_locations(method = "uniform",
                                              place_id = pid,
                                              n_house = large_num_houses,
                                              shapefile = delaware$shapefiles$shapefile, 
                                              noise = .001)
  
  # Verify the results are the correct class and equal length
  expect_true(class(uniform_locations) == "SpatialPoints")
  expect_true(length(uniform_locations_large) == large_num_houses)
})
