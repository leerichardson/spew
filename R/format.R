#' Format data for generation 
#' 
#' @param data_list list which contains all of the data from 
#' the read_data function 
#' @param data_group character vector indiciating which group 
#' the data is located in 
#' @return data_list list with an updated pop_table element which 
#' indicates the places in which we will generate synthetic ecosystems. 
#' The table should include three columns: the place_id, number of households 
#' to sample, and the puma id.  Note the the place_id should correspond to 
#' the place_id from the shapefile 
format_data <- function(data_list, data_group) {
    
  # Assert that we have all the REQUIRED (shapefile, pums, counts) elements 
  # for generating the synthetic population....
  if (is.null(data_list$pop_table) | is.null(data_list$shapefile) | is.null(data_list$pums)) {
    stop("data_list must contain pop_table, shapefile, and pums ")
  }
  
  
  if (data_group == "US") {

    # Make sure the place_id is the same type for merging 
    stopifnot(class(data_list$pop_table$place_id) == class(data_list$lookup$place_id))    
      
    # Pull out a vector of the poptable IDs and Shapefile IDs
    new_poptable <- plyr::join(data_list$pop_table, data_list$lookup,
                                 by = "place_id", type = "left")
    data_list$pop_table <- new_poptable
      
  } else if (data_group == "ipums") {
    
    # Pull out the names coming from the shapefile and counts 
    shapefile_names <- data_list$shapefiles$place_id
    level <- get_level(shapefile_names, data_list$pop_table)
    count_indices <- which(data_list$pop_table$level == level)
    count_names <- data_list$pop_table$place_id[count_indices]
    
    # Join the appropriate shapefile names, remove the commas 
    # from the counts, and determine the puma_id 
    shapefile_indices <- get_shapefile_indices(shapefile_names = shapefile_names, 
                                               count_names = count_names)
    
    no_commas <- gsub(pattern = ",", replacement = "", data_list$pop_table$n_house)
    new_nhouse <- as.numeric(no_commas)
    
    puma_id <- rep(NA, length(count_indices))
    
    # Create a revised pop-table and replace the old on in the data-list 
    new_poptable <- data.frame(place_id = shapefile_names[shapefile_indices], 
                                n_house = new_nhouse[count_indices], 
                                puma_id = puma_id)
    new_poptable$place_id <- as.character(new_poptable$place_id)
    
    classes <- unlist(lapply(new_poptable, class))
    stopifnot(!any(classes == "factor"))
    
    data_list$pop_table <- new_poptable
    
  } else if (data_group == "none") {
    
  }
  
  return(data_list)
}

#' Obtain the correct level for ipums data 
#' 
#' @param shapefile_names character vector of the names 
#' which come from the given shapefile 
#' @param pop_table data frame in which we want to obtain the 
#' levels 
#' @return level character of the level in which we will 
#' use 
get_level <- function(shapefile_names, pop_table) {
  num_places <- length(shapefile_names)
  levels <- table(pop_table$level)
  difs <- abs(num_places - levels)
  closest_dif <- as.vector(which(difs == min(difs)))
  level <- names(levels)[closest_dif]
  return(level)
}

#' Obtain the shapefile indices corresponding to the pop table 
#' 
#' @param shapefile_names character vector with the shapefile names  
#' @param count_names character vector with the count names 
#' @return numeric vector indicating the appropriate indices for 
#' shapefiles which correspond to the count_names 
get_shapefile_indices <- function(shapefile_names, count_names) {
  
  
  # Remove duplictae shapefile names 
  shapefile_names <- shapefile_names[!duplicated(shapefile_names)]
  count_names <- count_names[!duplicated(count_names)]
  stopifnot(length(shapefile_names) == length(count_names))
  
  # Remove the potential excess words 
  shapefile_names <- remove_excess_words(shapefile_names)
  count_names <- remove_excess_words(count_names)
  
  # Remove the non ascii characters, whitespaces, and 
  # uppercase letters 
  shapefile_names <- iconv(shapefile_names, to = "ASCII", sub = "")
  shapefile_names <- tolower(shapefile_names)
  shapefile_names <- gsub(pattern = " ", replacement = "", x = shapefile_names)
  
  count_names <- iconv(count_names, to = "ASCII", sub = "")
  count_names <- tolower(count_names)
  count_names <- gsub(pattern = " ", replacement = "", x = count_names)
  
  # Match the shapefile names against the count names. And make sure 
  # that both everything is matched and that 
  shapefile_indices <- amatch(shapefile_names, count_names, method = "jw", 
                              maxDist = .3)
  
  stopifnot(!any(is.na(shapefile_indices)))
  stopifnot(length(shapefile_indices) == length(shapefile_names))
  stopifnot(length(unique(shapefile_indices)) == length(shapefile_names))  
  return(shapefile_indices)
}

#' Remove extraneous words from place names 
#' 
#' @param names character vector of names   
#' @return names a character vector of updated names  
remove_excess_words <- function(names) {
  names <- gsub("Region del", "", names)
  names <- gsub("Region de", "", names)
  names <- gsub( " *\\(.*?\\) *", "", names)
  
  return(names)
}


