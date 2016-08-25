context("IPF")

test_that("Iterative Proportional Fitting", {
  # Load in the South Dakota data 
  data(sd_data)
  library(mipfp)
  
  # Households --------------- 
  index <- 1
  row <- sd_data$pop_table[index, ]
  
  ipf_households <- sample_households(method = "ipf", 
                                      n_house = row[, "n_house"], 
                                      pums_h = sd_data$pums$pums_h, 
                                      pums_p = sd_data$pums$pums_p, 
                                      marginals = sd_data$marginals,
                                      place_id = row[, "place_id"],
                                      puma_id = row[, "puma_id"])
  
})

