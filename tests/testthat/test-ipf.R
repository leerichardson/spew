context("IPF")

test_that("Iterative Proportional Fitting", {
  # Load in the South Dakota data 
  data(sd_data)
  library(mipfp)
  
  # Households --------------- 
  index <- 3
  row <- sd_data$pop_table[index, ]
  place_id <- row[, "place_id"]
  
  ipf_households <- sample_households(method = "uniform", 
                                      n_house = row[, "n_house"], 
                                      pums_h = sd_data$pums$pums_h, 
                                      pums_p = sd_data$pums$pums_p, 
                                      marginals = sd_data$marginals,
                                      place_id = row[, "place_id"],
                                      puma_id = row[, "puma_id"])
  
  srs_hh_inds <- sample(x = 1:nrow(sd_data$pums$pums_h), size = row[, "n_house"], replace = TRUE)
  srs_households <- sd_data$pums$pums_h[srs_hh_inds, ]
  
  table(srs_households$NP)
  table(ipf_households$NP)
  sd_data$marginals$NP$df[sd_data$marginals$NP$df$place_id == place_id, ]
})

