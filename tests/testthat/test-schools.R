context("School Functions")

test_that("United States School Assignment", {
  
  # Get the data for a randomly sampled tract set up ------------------------
  data(sd_data)
  index <- sample(x = 1:nrow(sd_data$pop_table), size = 1)
  
  # Obtain the specific parameters for this run of make 
  n_house <- sd_data$pop_table[index, "n_house"]
  puma_id <- sd_data$pop_table[index, "puma_id"]
  place_id <- sd_data$pop_table[index, "place_id"]
  
  # Sample n indices from the household pums 
  sampled_households <- sample_households(method = "uniform",
                                          n_house = n_house, 
                                          pums_h = sd_data$pums$pums_h, 
                                          pums_p = sd_data$pums$pums_p, 
                                          puma_id = puma_id, 
                                          place_id = place_id)
  
  # Attach locations to the sample households 
  locations <- sample_locations(method = "uniform", place_id = place_id, n_house = n_house, 
                                shapefile = sd_data$shapefile$shapefile)
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
                                  pums_p = sd_data$pums$pums_p, 
                                  puma_id = puma_id, 
                                  place_id = place_id)    
  sampled_people$place_id <- place_id
  sampled_people$puma_id <- puma_id
  
  # Schools --------------------------------------
  library(plyr)
  
  # Set up school assignment data-frame 
  school_var <- sampled_people$SCH
  school_grades <- sampled_people$SCHG
  school_ids <- assign_schools(sampled_people, sd_data$schools)
  ages <- sampled_people$AGEP
  
  school_df <- data.frame(enroll = school_var, grade = school_grades, 
                          age = ages, assignments = as.character(school_ids))
  school_df$assignments <- as.character(school_ids)

  # Make sure all of the school children are assigned to a
  # either public or private school 
  public_indices <- which(school_df$enroll == 2 & school_df$grade <= 14)
  public_schools <- school_df[public_indices, ]
  
  private_indices <- which(school_df$enroll == 3 & school_df$grade <= 14)
  private_schools <- school_df[private_indices, ]      

  expect_equal(any(is.na(public_schools$assignments)), FALSE)
  expect_equal(any(is.na(private_schools$assignments)), FALSE)
  
  # Make sure the non-school ID's aren't assigned to schools 
  non_school_indices <- which(school_df$enroll != 2 & school_df$enroll !=3)
  non_school_assignments <- school_df$assignments[non_school_indices]
  expect_equal(all(is.na(non_school_assignments)), TRUE)
})

