context("Sampling Methods")

test_that("Sampling Methods!", {
#   # Moment Matching ----------------------------
#   library(quadprog)
#   library(plyr)
#   data(sd_data)
# 
#   # Load in all of the necessary data 
#   sd_hh <- sd_data$pums$pums_h
#   sd_p <- sd_data$pums$pums_p
#   sd_poptab <- sd_data$pop_table
#   
#   # Subset the data to a particular PUMA 
#   puma <- 300
#   sd_hh <- sd_hh[which(sd_hh$puma_id == puma), ]
#   sd_p <- sd_p[which(sd_p$puma_id == puma), ]
#   sd_poptab <- sd_poptab[which(sd_poptab$puma_id == puma), ]
#   
#   # Obtain the weights 
#   M <- 2.42
#   n <- sort(unique(sd_hh$NP))
#   N <- length(n)
#   Q <- diag(N)
#   A <- t(rbind(rep(1, N), n, diag(N)))
#   dvec <- rep(0, N)
#   b <- c(1, M, rep(0, N))
#   meq <- 2
#   x <- solve.QP(Q, dvec, A, b, meq)
#   x <- x$solution
#   
#   # Determine the weights to assign to each individual 
#   # record based on the number of samples there are within 
#   # this particular microdata set 
#   hh_count <- as.numeric(table(sd_hh$NP))
#   hh_weights <- x / hh_count
#   hh_probs <- data.frame("NP" = n, hh_weights)
# 
#   # Merge the weights with the raw number of households 
#   hh_df <- data.frame(NP = sd_hh$NP)  
#   hh_df_merged <- join(x = hh_df, y = hh_probs, type = "left")
#   
#   # Sample from the weights established by moment matching 
#   inds <- sample(1:nrow(sd_hh), size = sd_poptab$n_house[1], replace = TRUE, prob = hh_df_merged$hh_weights)
#   result <- sd_hh[inds, ]  
#   mean(result$NP)
#   mean(sd_hh$NP)
#   
#   # Get confidence intervals for both SRS and MM methods 
#   bootstrap_samples <- 1000
#   srs_vec <- rep(0, bootstrap_samples)
#   mm_vec <- rep(0, bootstrap_samples)
#   
#   for (boot_samp in 1:bootstrap_samples) {
#     # Compute the average moment matching sample mean 
#     mm_inds <- sample(1:nrow(sd_hh), size = sd_poptab$n_house[1], replace = TRUE, prob = hh_df_merged$hh_weights)
#     mm_res <- sd_hh[mm_inds, ]
#     mm_mean <- mean(mm_res$NP)
#     mm_vec[boot_samp] <- mm_mean
#     
#     # Compute the corresponding SRS Sample 
#     srs_inds <- sample(1:nrow(sd_hh), size = sd_poptab$n_house[1], replace = TRUE)
#     srs_res <- sd_hh[srs_inds, ]
#     srs_mean <- mean(srs_res$NP)
#     srs_vec[boot_samp] <- srs_mean
#   }
#   
#   par(mfrow = c(1, 1))
#   hist(srs_vec, xlim = c(min(srs_vec), max(mm_vec)), 
#        main = "Distribution of Average Household Size for SRS and MM", 
#        xlab = "Average Household Size", col = "blue")
#   text(2, 200, labels = "SRS", cex = 2)
#   hist(mm_vec, xlim = c(min(srs_vec), max(mm_vec)), add = TRUE, col = "green")
#   text(2.35, 200, "MM", cex = 2)
#   abline(v = M, col = "red", lwd = 10)
  }
)
