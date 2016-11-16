## Moment Matching and helper functions

#' Sample households PUMS according to MM
#'
#' @param n_house number of households to sample 
#' @param pums_h household pums 
#' @param pums_p people pums, for appending on certain variables 
#' @param mm_obj list with moment matching data
#' @param puma_id id indicating the current puma 
#' @param place_id id indicating the current region
#' @param doSubsetPUMS logical.  When we do not need to subset the pums
sample_ipf <- function(n_house, pums_h, pums_p, mm_obj,
                       puma_id = NULL, place_id = NULL, doSubsetPUMS = TRUE){

    ## Step 0:  join PUMS
    ## Fill in
    pums <- TRUE
    
    ## Step 1:  Check if the mm_obj names are contained in the pums names
    stopifnot(check_mm_obj(mm_obj, pums_h))

    ## Step 2:  get the weights for the pums_h records
    mm_row <- getMMRow(place_id, puma_id, mm_obj)
    weights <- solveMMWeights(place_id = place_id, mm_row,
                              pums, assumption = mm_obj$assumption, mm_obj$meq)

    ## Step 3: sample households
    inds <- sample(1:nrow(pums), n_house, replace = T, prob = weights)
    households <- pums_h[inds,]
    return(households)
}


#' Weight the records of the PUMS so the averages in mm_df will be obtained
#'
#' @param place_id unique code identifying the place
#' @param mm_row dataframe of 1 row, includes place_id, puma_id, and averages, with names matching PUMS
#' @param pums the subsetted PUMS from which the sample will be drawn
#' @param assumption  "independence"
#' @return  x vector of length of the number of  records in the PUMS.  These are probabilities for each of the records.
#' @details the function solve.QP from the "quadprog" package is used.
solveMMWeights <- function(place_id, mm_row, pums, assumption = "independence", meq = 2){
    stopifnot(assumption %in% c("independence", "joint"))
    stopifnot(mm_row$place_id == place_id)
    stopifnot(sum(colnames(mm_row)[1:2] %in% c("puma_id", "place_id")) == 2)
     if(assumption == "independence"){
        p <- ncol(mm_row) - 2 # number of variables to determine weights
        weight_mat <- matrix(0, nrow = nrow(pums), ncol = p)
        for (var_ind in 3:ncol(mm_row)){
            weight_mat[, var_ind - 2] <- solveMMforVar(var_ind = var_ind, place_id, mm_row, pums, assumption, meq = 2)
        }
        x <- apply(weight_mat, 1, prod)
    } else if (assumption == "joint"){
        x <- solveMMforJoint(place_id, mm_row, pums, assumption, meq = 2)
    }
    x <- ifelse(x < 0, 0, x)
    x <- x /sum(x)
    stopifnot(all.equal(sum(x), 1))
    return(x)
}

#' Do the MM solving for joint distribution
#'
#' @param mm_row dataframe of 1 row, includes place_id, puma_id, and averages, with names matching PUMS
#' @param pums the subsetted PUMS from which the sample will be drawn
#' @param assumption  "joint"
#' @param meq (number of moments + 1) to match (2 is default and corresponds to matching the average)
#' @return  x vector of length of the number of records in the PUMS.  These are probabilities for each of the records in the PUMS.
solveMMforJoint <- function(place_id, mm_row, pums, assumption, meq = 2){
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
    p <- solve.QP(Q, dvec, A, b, meq)
    p <- p$solution
    x <- extrapolateProbsToPUMSjoint(p, n, pums, var_names, tab)
    return(x)
    }
    

#' Do the MM solving for an individual variable
#'
#' @param var_ind variable index of mm_row, colname should match one in PUMS.
#' @param mm_row dataframe of 1 row, includes place_id, puma_id, and averages, with names matching PUMS
#' @param pums the subsetted PUMS from which the sample will be drawn
#' @param assumption  "independence"
#' @param meq (number of moments + 1) to match (2 is default and corresponds to matching the average)
#' @return  x vector of length of the number of records in the PUMS.  These are probabilities for each of the records in the PUMS.
solveMMforVar <- function(var_ind, place_id, mm_row, pums, assumption, meq = 2){
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
    p <- solve.QP(Q, dvec, A, b, meq)
    p <- p$solution
    x <- extrapolateProbsToPUMS(p, n, pums, var_name)
    return(x)
}

#' Take unique probabilites for table and spread them to rest of PUMS
#'
#' @param p probabilities from solve.QP of length(unique(pums[, var_name]))
#' @param n vector with sorted unique categories.  p and n correspond to one another
#' @param pums PUMS data frame
#' @param var_name varaible name we are matching on
#' @return probabilities for whole data frame
extrapolateProbsToPUMS <- function(p, n, pums, var_name){
    counts <- sapply(1:length(p), function(ii){
        length(which(pums[, var_name]== n[ii]))
    })
    new_p <- p / counts
    x <- sapply(1:nrow(pums), function(ii){
        ind <- which(n == pums[ii, var_name])
        return(new_p[ind])
        })
    x <- x /sum(x)
    return(x)
}

#' Take unique probabilites for table and spread them to rest of PUMS
#'
#' @param p probabilities from solve.QP of length(unique(pums[, var_name]))
#' @param n matrix with unique categories  p and n correspond to one another
#' @param pums PUMS data frame
#' @param var_name varaible name we are matching on
#' @param tab data frame with categories and frequency
#' @return probabilities for whole data frame
extrapolateProbsToPUMSjoint <- function(p, n, pums, var_names, tab){
    new_p <- p / tab$Freq
    colnames(n) <- var_names
    small_df <- data.frame(n, p = new_p)
    df <- join(pums[, var_names], small_df)
    stopifnot(nrow(df) == nrow(pums))
    x <- df$p
    ## TODO
    ## Make a better join because it is not matching exactly.  Match on characters??
    x <- ifelse(is.na(x), 0, x)
    x <- x /sum(x)
    print(paste("We are sampling from, ", sum(x > 0), "unique PUMS records"))
    return(x)
}


#' Make mm object
#'
#' @param moments_list a list with each entry as a data frame.  The first df is the first moments, the second the second moments, etc.  Each df has the following format:  place_id | puma_id | var1 moment | var 2 moment |
#' @param assumption is either "independent" or "joint"
#' @param nMom number of moments
#' @param path if not NULL we will save this object
#' @return list of moment obj
make_mm_obj <- function(moments_list, assumption = "independent", nMom = 1, path = NULL){
    stopifnot(assumption %in% c("independent", "joint"))
    ll <- list(assumption = assumption, nMom = nMom, moments_list = moments_list)
    if(!is.null(path)){
        save(ll, path)
    }
    return(ll)
}
