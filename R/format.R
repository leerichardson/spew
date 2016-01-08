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
    
    
    # Make the n_house column numeric and the generic 
    no_commas <- gsub(pattern = ",", replacement = "", data_list$pop_table$n_house)
    new_nhouse <- as.numeric(no_commas)

    puma_id <- rep(NA, length(count_indices))
    
    # If there is an excess count in the population table, 
    # remove it and re-allocate the people evenly to other locations. 
    # Also need to update the shapefile_indices which are 
    # larger than the excess count... 
    if (!is.null(shapefile_indices$excess_count)) {
      new_nhouse <- allocate_count(new_nhouse, shapefile_indices$excess_count)
      sids <- which(shapefile_indices$shapefile_indices >= shapefile_indices$excess_count)
      shapefile_indices$shapefile_indices[sids] <- shapefile_indices$shapefile_indices[sids] - 1
    }
    
    # Create a revised pop-table and replace the old on in the data-list 
    new_poptable <- data.frame(place_id = shapefile_names[shapefile_indices$shapefile_indices], 
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
  
  # Check to see if there is an excess number of count names 
  if (length(count_names) > length(shapefile_names)) {
    warning("More count_names than shapefile names!")
    excess_count <- which(!(1:length(count_names) %in% shapefile_indices))  
  } else {
    excess_count <- NULL
  }
  
  # Make sure the shapefile indices are unique, have no missing
  # values, and that there is the same amount of these as count names 
  stopifnot(length(shapefile_names) + length(excess_count) == length(count_names))
  stopifnot(!any(is.na(shapefile_indices)))
  stopifnot(length(unique(shapefile_indices)) == length(shapefile_names))
  
  return(list(shapefile_indices = shapefile_indices, 
              excess_count = excess_count))
}

#' Remove extraneous words from place names 
#' 
#' @param names character vector of names   
#' @return names a character vector of updated names  
remove_excess_words <- function(names) {
  names <- gsub("Departamento del", "", names)
  names <- gsub("Departamento de", "", names)
  
  names <- gsub("Provincia del", "", names) 
  names <- gsub("Provincia de", "", names) 
  
  names <- gsub("Region de La", "", names)
  names <- gsub("Region del", "", names)
  names <- gsub("Region de", "", names)
  
  # Anything in between paranthesis (specifically for Chile)
  names <- gsub( " *\\(.*?\\) *", "", names)
  
  return(names)
}


#' Re-allocate excess counts to other locations 
#' 
#' @param counts numeric vector of current counts 
#' @param count_id numeric index indicating which 
#' count is excess
#' @return new_counts a new numeric count vector with the 
#' 
allocate_count <- function(counts, count_id) {
  to_allocate <- counts[count_id]
  to_add <- floor(to_allocate / length(counts))
  new_counts <- counts + to_add
  new_counts <- new_counts[-count_id]
  return(new_counts)
}

