#' Check the pop_table has all the necessary components 
#' 
#' @param pop_table 
#' @return Either a character "Pop table is ready!", or an 
#' error message detailing what went wrong 
check_pop_table <- function(pop_table) {
  pt_names <- names(pop_table)   
  
  stopifnot("place_id" %in% pt_names)
  stopifnot("n_house" %in% pt_names)
  stopifnot("puma_id" %in% pt_names)
  
  return("Pop table is ready!")
}

#' Check the shapefile has the necessary components 
#' 
#' @param shapefile 
#' @return Either a character"Shapefile is ready!" or an error message 
#' detailing what went wrong
check_shapefile <- function(shapefile) {
  shape_names <- names(shapefile)
  
  stopifnot("place_id" %in% shape_names)
  stopifnot(class(shapefile$place_id) == "character")
  
  return("Shapefile is ready!")
}

#' Check that the pums has all the required components 
#' 
#'  @param pums list with the household and person level 
#'  pums 
#'  @return  Either a character "Pums is ready!" or an error message 
#' detailing what went wrong
check_pums <- function(pums) {
  
  stopifnot(class(pums) == "list")
  
  stopifnot("pums_h" %in% names(pums))
  stopifnot("pums_p" %in% names(pums))
  
  stopifnot("puma_id" %in% names(pums$pums_h))

  stopifnot("SERIALNO" %in% names(pums$pums_h))
  stopifnot("SERIALNO" %in% names(pums$pums_p))
  
  # Make sure all people ID's are contained 
  # within a household 
  hh_serial <- pums$pums_h$SERIALNO
  p_serial <- pums$pums_p$SERIALNO
  stopifnot(all(p_serial %in% hh_serial))
  
  return("Pums is ready!")
}


#' Check the Place ID's match 
#' 
#' @param id1 character vector of place_ids 
#' @param id2 character vector of place_ids 
#' @return Either "Place ids match!" or an error
#' detailing why
check_place_ids <- function(id1, id2) {
  
  stopifnot(class(id1) == "character")
  stopifnot(class(id1) == class(id2))
  
  stopifnot(!any(duplicated(id1)))
  stopifnot(!any(duplicated(id2)))
  stopifnot(!any(is.na(id1)))
  stopifnot(!any(is.na(id2)))
  
  stopifnot(length(id1) == length(id2))
  
  return("Place ids match!")
}

#' Check the puma id's match  
#' 
#' @param pop_table_ids character vector of ids 
#' @param pums_ids character vector of ids
#' @return Either "Puma ids match!" or an error
#' detailing why
check_puma_ids <- function(pop_table_ids, pums_ids) {
  
  # If the pop_table ids don't match up with the 
  # puma ids, skip this because we are not subsetting pums 
  if (all(is.na(pop_table_ids))) {
    return("Puma ids match!")      
  }
  
  stopifnot(all(pop_table_ids %in% pums_ids))
  
  return("Puma ids match!")  
}
