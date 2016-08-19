#' Sample appropriate indices from household PUMS 
#' 
#' @param method character indicating the method for sampling 
#' @param n_house numeric indicating the number of households to sample 
#' @param pums_h dataframe of the households we are sampling from 
#' @param puma_id vector indicating which specific puma in PUMS we are sampling 
#' from, if any 
#' @return numeric with the indicies of the household PUMS to sample 
sample_households <- function(method, n_house, pums_h, puma_id = NULL, place_id = NULL) {
  if (method == "uniform") {  
    # Subset to a specific PUMA if we have data to do this 
    if (!is.na(puma_id)) {
      if (!(puma_id %in% unique(pums_h$puma_id))) {
        sample_inds <- 1:nrow(pums_h)
      }
      else {
        sample_inds <- which(pums_h$puma_id == puma_id)
        stopifnot(length(sample_inds) <= nrow(pums_h))
      }
    } else {
      sample_inds <- 1:nrow(pums_h)
    }
    
    households <- sample(sample_inds, n_house, replace = TRUE)
  }
  
  # Subset the sampled indices from the PUMS, and add 
  # in puma and place ids to the final pums 
  sampled_households <- pums_h[households, ]
  
  sampled_households$SYNTHETIC_HID <- paste0(place_id, "-", 1:nrow(sampled_households))
  stopifnot(!any(duplicated(sampled_households$SYNTHETIC_SERIAL)))  
  sampled_households$place_id <- place_id
  sampled_households$puma_id <- puma_id
  
  return(sampled_households)
}

#' Sample from the individual person PUMS data frame 
#' 
#' @param method character indicating the method for sampling 
#' @param household_pums dataframe with the sampled houehold PUMS 
#' @param pums_p dataframe of the individual microdata 
#' @return people numeric vector indicating the indices of people to sample 
sample_people <- function(method, household_pums, pums_p, puma_id = NULL, place_id = NULL) {
  if (method == "uniform") {
    sampled_people <- plyr::join(household_pums, pums_p, type = "left", by = "SERIALNO")
  }
  
  # Add in ids to the synthetic people and return 
  sampled_people$SYNTHETIC_PID <- paste0(sampled_people$SYNTHETIC_HID, "-", 1:nrow(sampled_people))
  stopifnot(!any(duplicated(sampled_people$SYNTHETIC_PID)))
  
  sampled_people$place_id <- place_id
  sampled_people$puma_id <- puma_id
  
  return(sampled_people)
}
