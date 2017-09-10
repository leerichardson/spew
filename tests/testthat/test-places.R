context("General Place Assignment Functions")

test_that("Place Assignment Functions", {
  # Check that dfs have longitude and latitude columns
  expect_false(checkDF(data.frame("longi" = 1, "latitude" = 0)))
  expect_true(checkDF(data.frame("longitude" = 1, "latitude" = 0)))
  expect_false(checkDF(data.frame("longitude" = 1)))

  # Checking get_dist_mat()
  pop <- data.frame(longitude = c(1, 2, 3), latitude = c(1, 2, 3))
  places <- data.frame(longitude = c(1, -3), latitude = c(1, -3))
  dist_mat <- get_dist_mat(pop, places)
  expect_equal(dist_mat[1, 1], 0)

  # check get_weight_dists
  weight_mat <- get_weight_dists(dist_mat, places, method = "uniform")
  expect_equal(dim(dist_mat), dim(weight_mat))
  expect_equal(rowSums(weight_mat), rep(1, nrow(weight_mat)))
  places$capacity <- c(10, 100)
  weight_mat <- get_weight_dists(dist_mat, places, method = "capacity")

  # Checking the sampling
  places$ID <- c("A", "B")
  place_inds <- apply(weight_mat, 1, function(row) sample(1:nrow(places), size = 1, prob = row))
  expect_true(length(place_inds) == nrow(pop))
  pop <- data.frame(pop, out = as.character(places[place_inds, "ID"]), stringsAsFactors = FALSE)

  # Get the data for a randomly sampled tract set up ------------------------
  data(delaware)
  index <- sample(x = 1:nrow(delaware$pop_table), size = 1)
  
  # Obtain the specific parameters for this run of make 
  n_house <- delaware$pop_table[index, "n_house"]
  puma_id <- delaware$pop_table[index, "puma_id"]
  place_id <- delaware$pop_table[index, "place_id"]
  
  # Sample n indices from the household pums 
  sampled_households <- sample_households(method = "uniform",
                                          n_house = n_house, 
                                          pums_h = delaware$pums$pums_h, 
                                          pums_p = delaware$pums$pums_p, 
                                          puma_id = puma_id, 
                                          place_id = place_id)
  
  # Attach locations to the sample households 
  locations <- spew:::sample_locations(method = "uniform", 
                                       place_id = place_id, 
                                       n_house = n_house, 
                                       shapefile = delaware$shapefile$shapefile)
  
  sampled_households$longitude <- locations@coords[, 1]
  sampled_households$latitude <- locations@coords[, 2]
  
  # Add a synthetic serial ID and place ID 
  # to the sampled households 
  sampled_households$SYNTHETIC_SERIAL <- 1:nrow(sampled_households)
  stopifnot(!any(duplicated(sampled_households$SYNTHETIC_SERIAL)))
  
  sampled_households$place_id <- place_id
  sampled_households$puma_id <- puma_id
  
  # Attach people to the sampled households and make 
  # sure to include both the place and puma id
  sampled_people <- sample_people(method = "uniform", 
                                  household_pums = sampled_households, 
                                  pums_p = delaware$pums$pums_p, 
                                  puma_id = puma_id, 
                                  place_id = place_id)  
  sampled_people$place_id <- place_id
  sampled_people$puma_id <- puma_id
  
  # Schools --------------------------------------
  library(plyr)
  
  # Set up school assignment data-frame 
  school_var <- sampled_people$SCH
  school_grades <- sampled_people$SCHG

  # Pop and place
  places <- delaware$schools$public
  names(places)[c(5,6)] <- c("longitude", "latitude")
  places$capacity <- places$Students
  pop <- sampled_households
  pid <- unique(pop$place_id)
  st <- substr(pid, 1, 2)
  co <- substr(pid, 3, 5)
  places2 <- places[ as.numeric(as.character(places$CoNo)) == as.numeric(co),]
  if (nrow(places) > 0) {
      places <- places2
  }
      
  expect_true(checkDF(pop))
  expect_true(checkDF(places))
  dist_mat <- get_dist_mat(pop, places)

  expect_true(min(dist_mat)  >= 0)
  expect_true(max(dist_mat) <= 1)

  # Testing weight_mat
  weight_mat <- get_weight_dists(dist_mat, places, method="capacity")
  expect_equal(dim(dist_mat), dim(weight_mat))

  # Testing whole function
  pop2 <- assign_place_coords(pop, places)
})



