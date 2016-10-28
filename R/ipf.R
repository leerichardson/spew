#' Sample households PUMS accoording to IPD 
#' 
#' @param n_house number of households to sample 
#' @param pums_h household pums 
#' @param pums_p people pums, for appending on certain variables 
#' @param marginals list with characteristics of marginals 
#' @param alpha number between 0 and 1, weight of categorical variables 
#' @param k number between 0 and 1, weight of orginal variables 
#' @param puma_id id indicating the current puma 
#' @param place_id id indicating the current region
sample_ipf <- function(n_house, pums_h, pums_p, marginals, alpha = 0, k = .001, 
                       puma_id = NULL, place_id = NULL) {
  # Step 1: Align PUMS with Marginals
  pums <- subset_pums(pums_h = pums_h, pums_p = pums_p, marginals = marginals, puma_id = puma_id)
  pums <- align_pums(pums, marginals)
  
  # Step 2: Fill in the contingency table
  table <- fill_cont_table(pums = pums, marginals = marginals, place_id = place_id, n_house = n_house)
  # Write out the contingency table HERE.
  
  # Step 3: Sample with contingency table weights 
  households <- sample_with_cont(pums = pums, table = table, alpha = alpha, 
                                 k = k, marginals = marginals)
  
  return(households)
}

#' Align pums with marginal totals 
#' 
#' @param pums_h dataframe of household pums 
#' @param pums_p dataframe of people pums
#' @param marginals list of marginals totals 
#' @param puma_id id for subsetting the pums 
#' 
#' @return pums with only relevant 
subset_pums <- function(pums_h, pums_p, marginals, puma_id) {
  # Subset PUMS to only marginal and ID variables ----
  var_names <- names(marginals)
  id_names <- c("SERIALNO", "puma_id", "RELP")
  household_inds <- which(names(pums_h) %in% c(id_names, var_names))
  people_inds <- which(names(pums_p) %in% c(id_names, var_names))
  stopifnot(length(c(household_inds, people_inds)) == length(var_names) + (2 * length(id_names)) - 1)
  
  pums_h_ipf <- pums_h[, household_inds]
  pums_p_ipf <- pums_p[, people_inds]
  stopifnot(all.equal(pums_h_ipf, pums_h[, household_inds]))
  
  # Only keep reference people in person level PUMS 
  pums_p_ipf <- pums_p_ipf[which(pums_p_ipf$RELP == 0), ]
  
  # Join the people variables onto the household vars 
  pums_h_ipf$row_index <- 1:nrow(pums_h_ipf)
  pums <- plyr::join(x = pums_h_ipf, y = pums_p_ipf, type = "left", by = "SERIALNO")
  stopifnot(pums$row_index == 1:nrow(pums_h_ipf))
  stopifnot(all.equal(pums[, 1:length(household_inds)], pums_h[, household_inds]))

  # Remove row index and return the subsetted pums 
  pums <- pums[, -which(duplicated(names(pums)))]
  pums <- pums[, which(names(pums) %in% c(var_names, id_names))]
  
  return(pums)
}

#' Match the pums variables with marginal totals 
#' 
#' @param pums dataframe subsetted to only the marginal vars
#' @param marginals list containing all of the marginal totals 
#' @param suffix what we add to the variable name to create the new variable name.  Default is "_marg"
#' @return pums dataframe with the marginal columns binded on
align_pums <- function(pums, marginals, suffix="_marg") {
  var_names <- names(marginals)
  # Loop through each marginal variable, extract the lookup 
  # table, create a new column with the marginal totals 
  # corresponding to the lookup, and add this column to pums 
  for (var in var_names) {
    marg <- marginals[[var]]
    marg_lookup <- marg$lookup
    num_cats <- nrow(marg_lookup)
    new_col <- rep(NA, nrow(pums))    
    
    # Create new column based on the marginal lookup 
    for (row in 1:num_cats) {
      lookup_row <- marg_lookup[row, ]
      marg_rows <- which(pums[, var] >= lookup_row$lower & pums[, var] <= lookup_row$upper)
      new_col[marg_rows] <- lookup_row$marg_names
    }
    # Turn the new column into an ordered factor to match 
    # the ordering of the marginal data-frame 
    new_col <- factor(new_col, levels = marg_lookup$marg_names)

    # Append new column to the pums 
    pums <- data.frame(pums, new_col)
    names(pums)[ncol(pums)] <- paste0(var, suffix)
  }
  
  return(pums)
}

#' Fill marginal contingency with ipf
#' 
#' @param pums dataframe of aligned pums data
#' @param marginals list with marginal totals 
#' @param place_id ID saying which column to extract 
#' the marginals from 
#' @param n_house number of households to sample
#' 
#' @return table a data-frame containing all of the 
#' marginal combinations and their frequencies 
fill_cont_table <- function(pums, marginals, place_id, n_house) {
  # Initialize the seed table 
  marg_col_inds <- grep(pattern = "_marg", names(pums))
  marg_cols <- pums[, marg_col_inds]
  seed <- table(marg_cols)
  
  # Extract and scale target_data and target_list 
  targets <- get_targets(marg_cols, marginals, place_id, n_house)
  target_data <- targets$target_data
  target_list <- targets$target_list

  # Fill contingency table with ipf 
  ipf_fit <- mipfp::Ipfp(seed = seed, target.list = target_list, target.data = target_data)
  ipf_tab <- ipf_fit$x.hat
  
# # Write out the IPF Tables for analysis 
# tab_loc <- file.path("/home/lee/Dropbox/ipf_midas/synthpop_ipf_unif/tables", paste0("table_", place_id))
# saveRDS(object = ipf_tab, file = tab_loc)
  
  table <- as.data.frame(ipf_tab)
  stopifnot(sum(table$Freq) - n_house < 1)

  # Round frequencies to whole numbers, update so
  # it matches the n_house total # of households 
  table$Freq <- round(table$Freq, digits = 0)
  while (sum(table$Freq) != n_house) {
    table$Freq <- update_freqs(table$Freq, n_house)
  }

  return(table)  
}

#' Update frequencies to match # of households 
#' 
#' @param frequencies 
#' @param n_house number of households 
update_freqs <- function(freqs, n_house) {
  n <- sum(freqs)
  update_ind <- sample(x = which(freqs > 0), size = 1, replace = TRUE)  
  if (n < n_house) {
    freqs[update_ind] <- freqs[update_ind] + 1
  } else if (n > n_house) {
    freqs[update_ind] <- freqs[update_ind] - 1
  }
  
  return(freqs)
}

#' Obtain the target marginals for IPF 
#' 
#' @param marg_cols dataframe of marginal columns 
#' @param marginals list of marginal data 
#' @param place_id id corresponding to the rows of marginals 
#' @param n_house number of households in this sample 
#' 
#' @return list with target_data and target list for this particular
#' place_id 
get_targets <- function(marg_cols, marginals, place_id, n_house) {
  number_vars <- ncol(marg_cols)
  target_data <- vector(mode = "list", length = number_vars)
  target_list <- vector(mode = "list", length = number_vars)
  
  var_names <- names(marg_cols)
  marg_names <- gsub(pattern = "_marg", "", x = var_names)
  for (var in marg_names) {
    # Extract the corresponding marginal row for this variable + place 
    marg_df <- marginals[[var]]$df
    place_row <- which(marg_df$place_id == place_id)
    marg_row <- marg_df[place_row, -1]

    # If all the marginals are 0, set the marginal row 
    # to the average for the state 
    if (all(marg_row == 0)) {
      marg_avg <- unlist(lapply(marg_df, mean)[-1])
      marg_row[1, ] <- marg_avg
    }
    
    # Scale the marginals by the n_house 
    marg_row_props <- as.numeric(marg_row) / sum(as.numeric(marg_row))
    marg_row_scaled <- n_house * marg_row_props
    
    # Update target data and list 
    dim <- which(var == marg_names)
    target_list[[dim]] <- dim
    target_data[[dim]] <- marg_row_scaled
  }
  # Verify that all the marginal data add up to the population total 
  stopifnot(all((unlist(lapply(target_data, sum)) - n_house) < 1))

  return(list(target_list = target_list, target_data = target_data))  
}

#' Sample from pums 
#' @param pums dataframe with marginal columns 
#' @param table dataframe with marginal combinations and frequencies 
#' @param alpha number between 0 and 1, weight of categorical variables 
#' @param k number between 0 and 1, weight of orginal variables 
#' @param marginals list with marginal data 
#' 
#' @return indices of sampled households 
sample_with_cont <- function(pums, table, alpha, k, marginals) {
  # Remove marginal combinations with 0 frequencies 
  table <- table[which(table$Freq != 0), ]
  
  # Create a weight matrix for every contingency 
  # table/pums row combination 
  weights <- assign_weights(pums = pums, table = table, alpha = alpha, 
                            k = k, marginals = marginals)
  
  # Sample the correct frequency for every non-zero marginal 
  # combination, using the weights 
  inds <- list()
  n <- nrow(table)
  
  for (i in 1:n) {  
    m <- table[i, "Freq"]
    m_weights <- weights[, i]
    inds[[i]] <- samp_ipf_inds(m, m_weights) 
  }
  inds <- unlist(inds)
  
  return(inds)  
}

samp_ipf_inds <- function(n, weights) {
  # Replace NA weights with 0's 
  weights[is.na(weights)] <- 0
  
  # Sample n inds using the weights 
  m <- length(weights)
  sample_inds <- sample(1:m, n, prob = weights, replace = TRUE)    
  stopifnot(length(sample_inds) == n)
  
  return(sample_inds)
}

#' Assign weights for ipf-based sampling 
#' 
#' @param pums dataframe with marginal columns 
#' @param table dataframe with marginal combinations and frequencies 
#' @param alpha number between 0 and 1, weight of categorical variables 
#' @param k number between 0 and 1, weight of orginal variables 
#' @param marginals list with marginal data 
#' 
#' @return weight matrix, rows = rows in pums, columns = rows in table 
assign_weights <- function(pums, table, alpha, k, marginals) {
  # Initialize a distance matrix 
  n <- nrow(pums)
  m <- nrow(table)
  dist_mat <- matrix(0, nrow = n, ncol = m)  
    
  # Calculate the weights for each column of the distance matrix 
  for (i in 1:m) {
    col_weights <- calc_dists(pums = pums, table_row = table[i, ], 
                              alpha = alpha, k = k, marginals = marginals)
    dist_mat[, i] <- col_weights
  }
  
  # Convert the distance matrix into probability matrix 
  pums_dist_sums <- colSums(dist_mat, na.rm = TRUE)
  sum_df <- matrix(rep(pums_dist_sums, n), ncol = m, byrow = TRUE)
  probs <- dist_mat / sum_df
  
  return(probs)
}

#' Calculate distance b/w cont table row and pums 
#' 
#' @param table_row 
calc_dists <- function(pums, table_row, alpha, k, marginals) {
  # Get the ordinal and categorical variable names 
  var_types <- lapply(marginals, function(x) x[["type"]])
  ord_vars <- names(which(var_types == "ord"))
  cat_vars <- names(which(var_types == "cat"))
  
  # Get ordinal distances 
  if (length(ord_vars) > 0) {
    ord_df <- sapply(1:length(ord_vars), get_ord_dists, ord_vars, table_row, pums, alpha, k)
  }
 
  # Get categorical distances 
  if (length(cat_vars) > 0) {
    cat_df <- sapply(1:length(cat_vars), get_cat_dists, cat_vars, table_row, pums, alpha, k)
  }
  
  # Combine ordinal and categorical distances and return   
  df <- cbind(ord_df, cat_df)
  dists <- apply(df, 1, prod)
  
  return(dists)
}

get_ord_dists <- function(ind, ord_vars, table_row, pums, alpha, k) {
  # Extract the orginal variable from pums 
  ord_var_name <- paste0(ord_vars[ind], "_marg")  
  pums_var <- pums[, ord_var_name]
  stopifnot(class(pums_var) == "factor")
  
  # Calculate the range of the ordinal variable 
  min_level <- min(as.integer(pums_var), na.rm = TRUE)
  max_level <- max(as.integer(pums_var), na.rm = TRUE)
  r <- max_level - min_level
  
  dip <- as.integer(pums_var)
  dic <- as.integer(table_row[ord_var_name])
  dist <- 1 - abs((dip - dic) / r)^k
     
  return(dist)
}

get_cat_dists <- function(ind, cat_vars, table_row, pums, alpha, k) {
  # Get PUMS rows of categorical val 
  cat_var_name <- paste0(cat_vars[ind], "_marg")
  dip <- as.character(pums[, cat_var_name])
  
  # Get valeu of cat val, calculate the distance and return 
  cat_val <- table_row[cat_var_name]
  dic <- lapply(cat_val, as.character)[[1]]
  
  dist <- ifelse(dip == dic, alpha, 1 - alpha) 
  dist <- 1 - dist
  
  return(dist)
}

