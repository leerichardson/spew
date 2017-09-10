context("IPF")

test_that("Iterative Proportional Fitting", {
  skip_if_not_installed("mipfp")
  library(mipfp)
  
  # Load in the South Dakota data 
  data(delaware)

  # Households ---
  index <- 3
  row <- delaware$pop_table[index, ]
  place_id <- row[, "place_id"]
  
  ipf_households <- sample_households(method = "ipf", 
                                      n_house = row[, "n_house"], 
                                      pums_h = delaware$pums$pums_h, 
                                      pums_p = delaware$pums$pums_p, 
                                      marginals = delaware$marginals,
                                      place_id = row[, "place_id"],
                                      puma_id = row[, "puma_id"])
  
  srs_hh_inds <- sample(x = 1:nrow(delaware$pums$pums_h), size = row[, "n_house"], replace = TRUE)
  srs_households <- delaware$pums$pums_h[srs_hh_inds, ]
  
  # Get the marginals of both methods, verify the mse for IPF is lower 
  true_marginals <- delaware$marginals$NP$df[delaware$marginals$NP$df$place_id == place_id, 2:8]
  true_marg <- c(0, as.vector(t(true_marginals)))
  
  srs_marg <- as.vector(table(srs_households$NP))
  srs_marg_07 <- srs_marg[1:8] 
  srs_marg_7p <- srs_marg[9:length(srs_marg)]
  srs_marg_07[8] <- srs_marg_07[8] + sum(srs_marg_7p) 
  
  ipf_marg <- c(0, as.vector(table(ipf_households$NP)), 0, 0)
  ipf_marg_07 <- ipf_marg[1:8]
  ipf_marg_7p <- ipf_marg[9:length(ipf_marg)]
  ipf_marg_07[8] <- ipf_marg[8] + sum(ipf_marg_7p)
  
  mse_srs <- sum((true_marg - srs_marg_07)^2)
  mse_ipf <- sum((true_marg - ipf_marg_07)^2)
  
  expect_true(mse_ipf < mse_srs)
})

