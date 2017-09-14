## Moment Matching and helper functions

#' Sample households PUMS according to MM
#'
#' @param n_house number of households to sample 
#' @param pums_h household pums 
#' @param pums_p people pums, for appending on certain variables 
#' @param mm_obj list with moment matching data
#' @param puma_id id indicating the current puma 
#' @param place_id id indicating the current region
#' 
sample_mm <- function(n_house, pums_h, pums_p, mm_obj, puma_id = NULL, place_id = NULL) {
    if (!requireNamespace("quadprog", quietly = TRUE)) {
      stop("quadprog needed for sample_mm to work.", call. = FALSE)
    }  
  
    if (n_house == 0) { return(NULL) }
  
    mom1_df <- mm_obj$moments_list$mom1
    if (sum(as.character(mom1_df$place_id) == place_id, na.rm = TRUE) < 1){ # there is no place_id  that matches the MM_OBJ
        
        ## Sample uniformly
        weights <- rep(1, nrow(pums_h)) / nrow(pums_h)
        pums <- pums_h
        
    } else{ # Do the MM procedure

        ## Step 1:  Check if the mm_obj names are contained in the pums names
        mm_vars <- colnames(mom1_df)[-which(colnames(mom1_df) %in% c("place_id", "puma_id"))]
        stopifnot(length(mm_vars) > 0 )
        pums_vars <- unique(c(colnames(pums_h), colnames(pums_p)))
        stopifnot(sum(mm_vars %in% pums_vars) == length(mm_vars))

        ## Step 2:  join PUMS
        marginals <- mom1_df[1, - which(colnames(mom1_df) %in% c("place_id", "puma_id"))]
        ## TODO:  FIX to be more general
        pums <- pums_h

        ## Step 3:  get the weights for the pums_h records
        ## TODO make functional for future moments
        mm_row <- mom1_df[which(as.character(mom1_df$place_id) == place_id),]
        weights <- solve_mm_weights(place_id = place_id, mm_row,
                                  pums, assumption = mm_obj$assumption, meq = (mm_obj$nMom + 1))
    }
    ## Step 4: sample household indices
    inds <- sample(1:nrow(pums), n_house, replace = T, prob = weights)
    return(inds)
}


#' Weight the records of the PUMS so the averages in mm_df will be obtained
#'
#' @param place_id unique code identifying the place
#' @param mm_row dataframe of 1 row, includes place_id, puma_id, and averages, with names matching PUMS
#' @param pums the subsetted PUMS from which the sample will be drawn
#' @param assumption  "independence"
#' @param meq (number of moments + 1) to match (2 is default and corresponds to matching the average)
#' 
#' @return  x vector of length of the number of  records in the PUMS.  These are probabilities for each of the records.
#' @details the function solve.QP from the "quadprog" package is used.
solve_mm_weights <- function(place_id, mm_row, pums, assumption = "independence", meq = 2){
    stopifnot(assumption %in% c("independence", "joint"))
    stopifnot(mm_row$place_id == place_id)
    stopifnot(sum(colnames(mm_row)[1:2] %in% c("puma_id", "place_id")) == 2)
     if(assumption == "independence"){
        p <- ncol(mm_row) - 2 # number of variables to determine weights
        weight_mat <- matrix(0, nrow = nrow(pums), ncol = p)
        for (var_ind in 3:ncol(mm_row)){
            weight_mat[, var_ind - 2] <- solve_mm_for_var(var_ind = var_ind, place_id, mm_row, pums, assumption, meq = 2)
        }
        x <- apply(weight_mat, 1, prod)
    } else if (assumption == "joint"){
        x <- solve_mm_for_joint(place_id, mm_row, pums, assumption, meq = 2)
    }
    x <- ifelse(x < 0, 0, x)
    x <- x /sum(x)
    stopifnot(all.equal(sum(x), 1))
    return(x)
}

#' Do the Moment Matching solving for joint distribution
#'
#' @param place_id unique code identifying the place
#' @param mm_row dataframe of 1 row, includes place_id, puma_id, and averages, with names matching PUMS
#' @param pums the subsetted PUMS from which the sample will be drawn
#' @param assumption  "joint"
#' @param meq (number of moments + 1) to match (2 is default and corresponds to matching the average)
#' 
#' @return  x vector of length of the number of records in the PUMS.  These are probabilities for each of the records in the PUMS.
solve_mm_for_joint <- function(place_id, mm_row, pums, assumption, meq = 2){
    M <- as.numeric(mm_row[, -c(1:2)])
    n_var <- ncol(mm_row) - 2
    var_names <- names(mm_row)[-c(1:2)]
    tab <- as.data.frame(table(pums[, colnames(mm_row[, -c(1:2)])]))
    tab <- tab[tab$Freq > 0,]
    n <- unlist(tab[, -which(colnames(tab) == "Freq")])
    n <- matrix(as.numeric(as.character(n)), ncol = n_var)
    N <- nrow(n)
    Q <- diag(N)
    A <- t(rbind(rep(1, N), t(n), diag(N)))
    dvec <- rep(0, N)
    b <- c(1, M, rep(0, N))
    meq <- 2
    
    p <- tryCatch({quadprog::solve.QP(Q, dvec, A, b, meq)$solution}, error = function(e){
        print(e)
        p <- rep(1, length(nrow(tab)))
        return(p)
    })
    
    x <- extrapolate_probs_to_pums_joint(p, n, pums, var_names, tab)
    return(x)
    }
    

#' Do the MM solving for an individual variable
#'
#' @param var_ind variable index of mm_row, colname should match one in PUMS.
#' @param place_id unique code identifying the place
#' @param mm_row dataframe of 1 row, includes place_id, puma_id, and averages, with names matching PUMS
#' @param pums the subsetted PUMS from which the sample will be drawn
#' @param assumption  "independence"
#' @param meq (number of moments + 1) to match (2 is default and corresponds to matching the average)
#' @return  x vector of length of the number of records in the PUMS.  These are probabilities for each of the records in the PUMS.
solve_mm_for_var <- function(var_ind, place_id, mm_row, pums, assumption, meq = 2){
    var_name <- colnames(mm_row)[var_ind]
    ## The below notation follows that of solve.QP.  See help file for more details
    M <- mm_row[var_ind] # the average value of var_name for this tract
    n <- sort(unique(pums[, var_name]))
    N <- length(n)
    Q <- diag(N)
    A <- t(rbind(rep(1, N), n, diag(N)))
    dvec <- rep(0, N)
    b <- c(1, M, rep(0, N))
    meq <- 2
    p <- tryCatch({quadprog::solve.QP(Q, dvec, A, b, meq)$solution}, error = function(e){
        print(e)
        p <- rep(1, N)
        return(p)
                  })
    x <- extrapolate_probs_to_pums(p, n, pums, var_name)
    return(x)
}

#' Take unique probabilites for table and spread them to rest of PUMS
#'
#' @param p probabilities from solve.QP of length(unique(pums[, var_name]))
#' @param n vector with sorted unique categories.  p and n correspond to one another
#' @param pums PUMS data frame
#' @param var_name varaible name we are matching on
#' 
#' @return probabilities for whole data frame
extrapolate_probs_to_pums <- function(p, n, pums, var_name){
    counts <- sapply(1:length(p), function(ii){
        length(which(pums[, var_name]== n[ii]))
    })
    new_p <- p / counts
    x <- sapply(1:nrow(pums), function(ii){
        ind <- which(n == pums[ii, var_name])
        return(new_p[ind])
        })
    x <- x / sum(x)
    return(x)
}

#' Take unique probabilites for table and spread them to rest of PUMS
#'
#' @param p probabilities from solve.QP of length(unique(pums[, var_name]))
#' @param n matrix with unique categories  p and n correspond to one another
#' @param pums PUMS data frame
#' @param var_names varaible name we are matching on
#' @param tab data frame with categories and frequency
#' @return probabilities for whole data frame
extrapolate_probs_to_pums_joint <- function(p, n, pums, var_names, tab){
    new_p <- p / tab$Freq
    colnames(n) <- var_names
    small_df <- data.frame(n)
    small_df <- as.data.frame(apply(small_df, 2, as.character), stringsAsFactors = FALSE)
    small_df$p <- new_p
    pums_sub <- pums[, var_names]
    pums_sub <- as.data.frame(apply(pums_sub, 2, as.character), stringsAsFactors = FALSE)
    df <- join(pums_sub, small_df)
    stopifnot(nrow(df) == nrow(pums))
    x <- as.numeric(df$p)
    ## TODO
    ## Make a better join because it is not matching exactly.  Match on characters??
    x <- ifelse(is.na(x), 0, x)
    x <- x / sum(x)
    
    return(x)
}


#' Make moment matching object
#'
#' @param moments_list a list with each entry as a data frame.  The first df is the first moments, the second the second moments, etc.  Each df has the following format:  place_id | puma_id | var1 moment | var 2 moment |.  The dfs are named mom1, mom2, ...
#' @param assumption is either "independent" or "joint".  "independent" assumes that the distributions of the characteristics are independent of one another.  "joint" means we use the empirical distribution of the microdata when finding weights in moment matching.
#' @param nMom number of moments.  Currently, we only support the first moment, e.g. the average of a variable.  Default is 1.
#' @param type either "cont" for continuous variable or "ord" for ordered variable
#' @param region identifier for region
#' @param path if not NULL we will save this object to the specified path as a RDS object.
#' @return list of moment object
#' @export
make_mm_obj <- function(moments_list, assumption = "independence", nMom = 1,
                      type = "cont", region = NULL, path = NULL){
    stopifnot(assumption %in% c("independence", "joint"))
    mm_obj<- list(assumption = assumption, nMom = nMom, type = type, region = region, moments_list = moments_list)
    if(!is.null(path)){
        saveRDS(mm_obj, file = path)
    }
    return(mm_obj)
}

#' Impute Missing Values in a data frame
#'
#' @param df where ever column may be imputed (so each column is numeric)
#' @param method of imputation, "mean" imputes the mean of the other values into the NA values.  "bootstrap" resamples from non-NA vals.
#' @return df of same dimension, now with no missing vals
#' @export
impute_missing_vals <- function(df, method = "mean"){
    ## TODO  add in joint distribution imputations
    dimInit <- dim(df)
    if(method == "mean"){
        df <- apply(df, 2, function(col) {
            missing_inds <- which(is.na(col))
            mn <- mean(col, na.rm = TRUE) 
            col[missing_inds] <- mn
            return(col)
        })

    } else if (method == "bootstrap"){
         df <- apply(df, 2, function(col) {
             missing_inds <- which(is.na(col))
             if(length(missing_inds) < 1){
                 return(col)
             }
            missing_vals <- col[sample(1:length(col)[-missing_inds], length(missing_inds), replace = TRUE)]
            col[missing_inds] <- missing_vals
            return(col)
         })
    }
    stopifnot(all(dimInit == dim(df)))
    return(df)
}
