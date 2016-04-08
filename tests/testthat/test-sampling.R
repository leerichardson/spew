context("Sampling Methods")

test_that("Sampling Methods!", {
  
  # Density Estimation ----------------------
  
  # Load in all of the necessary data 
  data(sd_data)
  sd_hh <- sd_data$pums$pums_h
  sd_p <- sd_data$pums$pums_p
  sd_poptab <- sd_data$pop_table
  
  # Subset the data to a particular PUMA 
  puma <- 300
  sd_hh <- sd_hh[which(sd_hh$puma_id == puma), ]
  sd_p <- sd_p[which(sd_p$puma_id == puma), ]
  sd_poptab <- sd_poptab[which(sd_poptab$puma_id == puma), ]
  
  # Estimate the Household Size distribution 
  hh_sizes <- sd_hh$NP
  
  # Estimate the Head of Household Distribution 
  
  
  }
)
