context("Sampling Methods")

test_that("Moment Matching", {
  # Moment Matching
  skip_if_not_installed("quadprog")
  library(quadprog)
  library(plyr)
  data(delaware)

  # Load in all of the necessary data 
  del_hh <- delaware$pums$pums_h
  del_p <- delaware$pums$pums_p
  del_poptab <- delaware$pop_table
  
  # Subset the data to a particular PUMA 
  puma <- 300
  del_hh <- del_hh[which(del_hh$puma_id == puma), ]
  del_p <- del_p[which(del_p$puma_id == puma), ]
  del_poptab <- del_poptab[which(del_poptab$puma_id == puma), ]
  pums <- del_hh

  # Testing functions
  place_id <- "SG1"
  puma_id <- "LFR"
  mm_row <- data.frame(place_id = place_id, puma_id = puma_id, NP = 2.8)
  pums <- del_hh
  var_ind <- 3
  var_name <- "NP"
  counts <- 10^5
  pums_probs <- solve_mm_weights(place_id, mm_row, pums)
  inds <- sample(1:nrow(pums), counts, replace = T, prob = pums_probs)
  stopifnot(abs(2.8 -     mean(pums[inds, ]$NP)) < .05)
  
  # Trying hincp
  pums$HINCP[is.na(pums$HINCP)] <- mean(pums$HINCP, na.rm = T)
  counts <- 10^5
  mm_row <- data.frame(place_id = place_id, puma_id = puma_id, HINCP = 50000)
  pums_probs <- solve_mm_weights(place_id, mm_row, pums)
  inds <- sample(1:nrow(pums), counts, replace = T, prob = pums_probs)
  stopifnot(abs(50000 - mean(pums[inds, ]$HINCP) ) < 1000)

  # Both with joint distribution!!
  counts <- 10^4
  pums$HINCP[is.na(pums$HINCP)] <- mean(pums$HINCP, na.rm = T)
  mm_row <- data.frame(place_id = place_id, puma_id = puma_id,  NP = 1.1, HINCP = 41000)
  pums_probs <- solve_mm_weights(place_id, mm_row, pums, assumption = "joint")
  inds <- sample(1:nrow(pums), counts, replace = T, prob = pums_probs)
  stopifnot(abs(41000 - mean(pums$HINCP[inds])) < 5000)
  stopifnot(abs(1.1 - mean(pums$NP[inds])) < .1)
})
