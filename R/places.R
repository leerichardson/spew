#' Assign a place with long/lat coords to a synthetic population
#'
#' @param pop data frame with "longitude" and "latitude" columns
#' @param places data frame with "longitude" and "latitude" and an "ID" column, perhaps a "capacity" column
#' @param place_name string that will become the column name of the place
#' @param method c("uniform", "capacity") The method on how we assign places
#' @param dist_fxn (haversine_dist) or a function with args x1, y1, x2, y2
#' @return pop data frame with column of place_name with the place_ids
assign_place_coords<- function(pop, places, place_name ="place", method = "uniform", dist_fxn = euclidean_dist){

    # First check if the pop df and the places df are in the right format
    stopifnot(checkDF(pop, type = "coords"))
    stopifnot(checkDF(places, type = "coords"))
    stopifnot(nrow(places) > 0)
    stopifnot(nrow(pop) > 0)

    # Get the distance matrix
    dist_mat <- get_dist_mat(pop, places, dist_fxn)

    # Scale distance mat between 0 and 1 for each row
    dist_mat <- t(apply(dist_mat, 1, function(row){
        out <- (row - min(row, na.rm = TRUE) ) /
            (max(row, na.rm = TRUE) - min(row,
                                               na.rm = TRUE)  + .001)
        return(out)
            }))

    # If there is no capacity column, do uniform sampling
    if(!("capacity" %in% names(places))){
        method <- "uniform"
    }

    weight_mat <- get_weight_dists(dist_mat, places, method)

    # Use weights to assign place
    place_inds <- apply(weight_mat, 1, function(row) sample(1:nrow(places), size = 1, prob = row))

    # Attach the assigned places to the pop df
    stopifnot(length(place_inds) == nrow(pop))
    stopifnot("ID" %in% names(places))
   
    pop <- data.frame(pop, out = as.character(places[place_inds, "ID"]), stringsAsFactors = FALSE)

    # Rename the column
    names(pop)[ncol(pop)] <- place_name
    return(pop)
    
}

#' Check if df is in the right format
#' 
#' @param df data frame 
#' @param type ("coords").  For type "coords", we check to make sure "longitude" and "latitude" are column names
#' @return logical
checkDF <- function(df, type = "coords"){
    if (type == "coords"){
        didPassChecks <- all( c("latitude", "longitude") %in% names(df))
    }
    return(didPassChecks)
}

#' Get the distance matrix 
#'
#' Get the distance matrix between two data frames that have "longitude" and "latitude" columns
#' @param pop data frame of the population with m rows
#' @param places data frame of places to assign with n rows
#' @param dist_fxn currently "haversine" with args for x1, y1, x2, y2, returning a scalar value between 0 and 1
#' @return m x n matrix with scaled distance between 0 and 1.  Eg.  Entry ij means that the scaled distance between row i from pop and row j from places is entry ij.
get_dist_mat <- function(pop, places, dist_fxn = haversine_dist){
    m <- nrow(pop) # number of individuals in pop
    n <- nrow(places) # number of places

    # Find the distance between each individual and each place
    dist_mat <- apply(pop, 1, function(row){
        x1 <- as.numeric(as.character(rep(row['longitude'], n)))
        stopifnot(!is.null(row['longitude']))
        y1 <- as.numeric(as.character(rep(row['latitude'], n)))
        x2 <- as.numeric(as.character(places$longitude))
        y2 <- as.numeric(as.character(places$latitude))
        dists <- dist_fxn(x1, y1, x2, y2)
        return(dists)
    })
    
    if (m == 1) { # seems mismatched because we transpose
        dist_mat <- matrix(dist_mat, ncol=1)
    } else if (n == 1) {
        dist_mat <- matrix(dist_mat, nrow=1)
    }
    stopifnot(dim(t(dist_mat)) == c(m,n))
    return(t(dist_mat))
}

#' Get the haversine distance between two points (x1, y1) and (x2, y2) scaled between 0 and 1.
#' 
#' @param x1 longitude of object 1 (vector)
#' @param y1 latitude of object 1 (vector)
#' @param x2 longitude of object 2 (vector)
#' @param y2 latitude of object 2 (vector)
#' @references http://andrew.hedges.name/experiments/haversine/
#' @return numeric 
haversine_dist <- function(x1, y1, x2, y2){
  dx <- x2 - x1 
  dy <- y2 - y1 
  a <- (sin(dy/2))^2 + cos(y1) * cos(y2) * (sin(dx/2))^2
  d <- atan2( sqrt(a), sqrt(1-a))
  d <- (d / pi)
  stopifnot( all(d >= 0 ))
  stopifnot( all( d <= 1))
  return(d)
}

#' Get the euclidean distance between two points (x1, y1) and (x2, y2) 
#' 
#' @param x1 longitude of object 1 (vector)
#' @param y1 latitude of object 1 (vector)
#' @param x2 longitude of object 2 (vector)
#' @param y2 latitude of object 2 (vector)
#' @return numeric 
euclidean_dist <- function(x1, y1, x2, y2){
    d <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
    return(d)
}

#' Weight place assignment probabilities  
#'
#' @param dist_mat a m x n matrix where m is the number of people and 
#' n is the number of schools
#' @param places data frame of places with an ID column
#' @param method ("uniform", "capacity")
#' @return m x n matrix of probabilities.  Each row should sum to 1
get_weight_dists <- function(dist_mat, places, method="uniform"){
  m <- nrow(dist_mat)
  n <- ncol(dist_mat)

  # If uniform sampling, we assume each place has the same capacity
  if(method == "uniform"){
      places$capacity <- 100
  }
  stopifnot("capacity" %in% names(places))

  capacity <- as.numeric(as.character(places$capacity))
  stopifnot(length(capacity) == n)
  
  places_weight <- ceiling(capacity/20)
  places_weight <- ifelse(is.na(places_weight), 1, places_weight)
  places_weight_mat <- matrix(rep(places_weight, each = m), nrow = m)
  dist_mat <- dist_mat
  weights <- (1-dist_mat) * places_weight_mat
  weights <- ifelse(is.na(weights),
                    .01, weights)
  weights <- weights/rowSums(weights)
  stopifnot(dim(weights) == c(m,n))
  return(weights)
}
