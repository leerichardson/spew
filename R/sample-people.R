#' Sample appropriate indices from household PUMS 
#' 
#' @param method character indicating the method for sampling 
#' @param n_house numeric indicating the number of households to sample 
#' @param pums_h dataframe of the households we are sampling from 
#' @param pums_p optionally include a dataframe of agents 
#' @param puma_id vector indicating which specific puma in PUMS we are sampling 
#' from, if any 
#' @param place_id unique code identifying the place
#' @param marginals optionally used for IPF or MM sampling 
#' 
#' @export
#' 
#' @return numeric with the indicies of the household PUMS to sample
#' 
#' @examples
#' data("tartanville")
#' example_place_id <- tartanville$pop_table$place_id[1] 
#' example_puma_id <- tartanville$pop_table$puma_id[1] 
#' example_n_house <- tartanville$pop_table$n_house[1] 
#' sample_households(method = "uniform", 
#'                   pums_h = tartanville$pums_h, 
#'                   pums_p = tartanville$pums_p, 
#'                   n_house = example_n_house, 
#'                   place_id = example_place_id, 
#'                   puma_id = example_puma_id)
#' 
sample_households <- function(method, n_house, pums_h, pums_p = NULL,
                              puma_id = NULL, place_id = NULL, 
                              marginals = NULL) {
  if (method == "uniform") {
    households <- sample_uniform(n_house, pums_h, puma_id = puma_id, place_id = place_id)
    
  } else if (method == "ipf") {
    households <- sample_ipf(n_house = n_house, pums_h = pums_h, pums_p = pums_p, 
                             puma_id = puma_id, place_id = place_id, 
                             marginals = marginals)
    
  } else if (method == "mm") {
      households <- sample_mm(n_house = n_house, pums_h = pums_h, pums_p = pums_p, 
                              mm_obj = marginals, puma_id = puma_id, place_id = place_id)
      
  } else {
    stop("Sampling method must be ipf, mm, or uniform")
  }
  
  # Subset the sampled indices from the PUMS, and add in puma and place ids to the final pums 
  sampled_households <- pums_h[households, ]
  
  # Remove the comma's from ourput
  place_id <- remove_excess(place_id)
  puma_id <- remove_excess(puma_id)
  sampled_households$place_id <- place_id
  sampled_households$puma_id <- puma_id
  
  # Create the Synthetif HID and add this as well as place/puma to the population. 
  sampled_households$SYNTHETIC_HID <- paste0(place_id, "-", 1:nrow(sampled_households))
  stopifnot(!any(duplicated(sampled_households$SYNTHETIC_HID)))  
    
  return(sampled_households)
}

#' Sample from the individual person PUMS data frame 
#' 
#' @param method character indicating the method for sampling 
#' @param household_pums dataframe with the sampled houehold PUMS 
#' @param pums_p dataframe of the individual microdata 
#' @param puma_id ID of microdata sampling region 
#' @param place_id ID of place 
#' 
#' @export
#' 
#' @return people numeric vector indicating the indices of people to sample 
sample_people <- function(method, household_pums, pums_p, puma_id = NULL, place_id = NULL) {
  if (method %in%  c("uniform", "ipf", "mm")) {
    # Don't duplucate variables, except lat/lon which is required for environmental assignment 
    household_pums <- household_pums[, c("SERIALNO", "SYNTHETIC_HID", "latitude", "longitude")]
    
    # Include all people from the sampled households 
    sampled_people <- plyr::join(household_pums, pums_p, type = "left", by = "SERIALNO")
    
  } else { stop("Sampling method must be ipf, mm, or uniform") }
  
  # Remove names which comp
  place_id <- remove_excess(place_id)
  puma_id <- remove_excess(puma_id)
  sampled_people$place_id <- place_id
  sampled_people$puma_id <- puma_id  
  
  # Add in a person synthetic ID 
  sampled_people$SYNTHETIC_PID <- paste0(sampled_people$SYNTHETIC_HID, "-", 1:nrow(sampled_people))
  stopifnot(!any(duplicated(sampled_people$SYNTHETIC_PID)))
  
  return(sampled_people)
}

#' Sample households uniformly 
#' 
#' @param n_house number of households to sample 
#' @param pums_h the household pums 
#' @param puma_id if specifying the subset of PUMS to sample s
#' @param place_id id of the current region 
#' 
#' @export
#' 
sample_uniform <- function(n_house, pums_h, puma_id = NULL, place_id = NULL) {
  # Subset to a specific PUMA if we have data to do this 
  if (!is.null(puma_id)) {
    if (!(puma_id %in% unique(pums_h$puma_id))) {
      sample_inds <- 1:nrow(pums_h)
    } else {
      sample_inds <- which(pums_h$puma_id == puma_id)
      stopifnot(length(sample_inds) <= nrow(pums_h))
    }
  } else {
    sample_inds <- 1:nrow(pums_h)
  }
    
  # Sample households uniformly with replacement 
  households <- sample(sample_inds, n_house, replace = TRUE)
  return(households)
}

#' Remove comma's, accents, etc. from name 
#' 
#' @param name character 
#' @return name with all of the excess baggage removed 
remove_excess <- function(name) {
  name <- gsub(",", "-", name)
  name <- gsub("\r", "", name)
  name <- gsub("\n", "", name)  
  return(name)
}
