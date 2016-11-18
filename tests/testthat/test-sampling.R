context("Sampling Methods")

test_that("Sampling Methods!", {
    ## Moment Matching
    library(quadprog)
    library(plyr)
    data(sd_data)

  # Load in all of the necessary data 
  sd_hh <- sd_data$pums$pums_h
  sd_p <- sd_data$pums$pums_p
  sd_poptab <- sd_data$pop_table
  
  # Subset the data to a particular PUMA 
  puma <- 300
  sd_hh <- sd_hh[which(sd_hh$puma_id == puma), ]
  sd_p <- sd_p[which(sd_p$puma_id == puma), ]
  sd_poptab <- sd_poptab[which(sd_poptab$puma_id == puma), ]
  pums <- sd_hh

    ## Testing functions
    place_id <- "SG1"
    puma_id <- "LFR"
    mm_row <- data.frame(place_id = place_id, puma_id = puma_id, NP = 2.8)
    pums <- sd_hh
    var_ind <- 3
    var_name <- "NP"
    counts <- 10^5
    pums_probs <- solve_mm_weights(place_id, mm_row, pums)
    inds <- sample(1:nrow(pums), counts, replace = T, prob = pums_probs)
    stopifnot(abs(2.8 -     mean(pums[inds, ]$NP)) < .05)

    
    ## Trying hincp
    pums$HINCP[is.na(pums$HINCP)] <- mean(pums$HINCP, na.rm = T)
    counts <- 10^5
    mm_row <- data.frame(place_id = place_id, puma_id = puma_id, HINCP = 50000)
    pums_probs <- solve_mm_weights(place_id, mm_row, pums)
    inds <- sample(1:nrow(pums), counts, replace = T, prob = pums_probs)
    stopifnot(abs(50000 - mean(pums[inds, ]$HINCP) ) < 1000)

    ## Both with joint distribution!!
    counts <- 10^4
    pums$HINCP[is.na(pums$HINCP)] <- mean(pums$HINCP, na.rm = T)
    mm_row <- data.frame(place_id = place_id, puma_id = puma_id,  NP = 1.1, HINCP = 41000)
    pums_probs <- solve_mm_weights(place_id, mm_row, pums, assumption = "joint")
    inds <- sample(1:nrow(pums), counts, replace = T, prob = pums_probs)
    stopifnot(abs(41000 - mean(pums$HINCP[inds])) < 5000)
    stopifnot(abs(1.1 - mean(pums$NP[inds])) < .1)




})
