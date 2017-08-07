#' Format data before entering make 
#' 
#' @param data_list list which contains all of the data from 
#' the read_data function 
#' @param data_group character vector indiciating which group 
#' the data is located in 
#' @param verbose whether to print out the timings 
#' 
#' @return data_list list with an updated pop_table element which 
#' indicates the places in which we will generate synthetic ecosystems.
#' The table should include three columns: the place_id, number of households 
#' to sample, and the puma id.  Note the the place_id should correspond to 
#' the place_id from the shapefile 
format_data <- function(data_list, data_group, verbose = TRUE) {
  format_start_time <- Sys.time()
  
  # Assert that we have all the REQUIRED (shapefile, pums, counts) elements 
  # for generating the synthetic population....
  if (is.null(data_list$pop_table) | is.null(data_list$shapefile) | is.null(data_list$pums)) {
    stop("data_list must contain pop_table, shapefile, and pums")
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
    # PUMA id as an NA, since we are not yet able to subset
    no_commas <- gsub(pattern = ",", replacement = "", data_list$pop_table$n_house)
    new_nhouse <- as.numeric(no_commas)
    
    # Update the PUMA_ID's 
    puma_ids <- as.numeric(as.character(data_list$shapefiles$puma_id))  
    
    # Create a revised pop-table and replace the old on in the data-list 
    new_poptable <- data.frame(place_id = shapefile_names[shapefile_indices], 
                               n_house = new_nhouse[count_indices], 
                               puma_id = puma_ids[shapefile_indices])
    new_poptable$place_id <- as.character(new_poptable$place_id)
    
    # Assert that we have only numeric and character classes 
    classes <- unlist(lapply(new_poptable, class))
    stopifnot(all(classes %in% c("character", "numeric")))
    
    # Update the pop_table with the revised table 
    data_list$pop_table <- new_poptable
    
  } else if (data_group == "none") {
    # Check all of the locations match up     
    pop_table_places <- data_list$pop_table$place_id
    pop_table_pumas <-  data_list$pop_table$puma_id
    shapefile_places <- data_list$shapefiles$place_id
    pums_pumas <- data_list$pums$pums_h$puma_id
    
    check_place_ids(pop_table_places, shapefile_places)
    check_puma_ids(pop_table_pumas, pums_pumas)
  }
  
  # Print out the length of time it takes for format to run
  format_time <- difftime(Sys.time(), format_start_time, units = "secs")
  format_time <- as.numeric(round(format_time, digits = 2))  
  format_time_statement <- paste0("Format runs in: ", format_time) 
  if (verbose) { print(format_time_statement) }
  
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
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("stringdist needed for function 'get_shapefile_indices' to work.", call. = FALSE)
  }
  
  # Match the shapefile names against the count names
  shapefile_indices <- stringdist::amatch(count_names, shapefile_names, method = "jw", maxDist = .3)
  
  # Make sure the shapefile indices are unique, have no missing
  # values, and that there is the same amount of these as count names 
  stopifnot(!any(is.na(shapefile_indices)))
  stopifnot(!any(duplicated(shapefile_indices)))
  
  return(shapefile_indices)
}

#' Remove whitespace, capitals, and non ASCII
#' 
#' @param names character vector of names to clean 
#' @return names character vector of all lowercase, 
#' non-capital and ASCII names 
clean_names <- function(names) {
  names <- iconv(names, to = "ASCII", sub = "")
  names <- tolower(names)
  names <- gsub(pattern = " ", replacement = "", x = names)
  return(names)
}

#' Replace an existing word 
#' 
#' @param word character of the word you want to replace 
#' @param replace character of what you want to replace 
#' the word 
#' @param names character vector of words which we can
#' replace 
#' 
#' @return names character vector of the replaced word 
replace_word <- function(word, replace, names) {
  index <- which(names == word)
  names[index] <- replace
  return(names)
}

#' Remove excess words
#' 
#' @param word character of the word you want to replace 
#' @param names character vector of the words 
#' we can potentially replace 
#' 
#' @return names character vector with the excess 
#' word removed 
remove_words <- function(word, names) {
  names <- gsub(word, "", names)
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

#' Combine two rows of a pop_table into one 
#' 
#' @param pop_table dataframe to update 
#' @param place1 character indicating the name of the 
#' place to add counts to 
#' @param place2 character indicating the name of the 
#' place to remove 
#' 
#' @return pop_table a data-frame with the place2 counts 
#' added to place1, and place2 removed from the pop_table 
combine_counts <- function(pop_table, place1, place2) {
  place1_index <- which(pop_table$place_id == place1)
  place2_index <- which(pop_table$place_id == place2)
  
  place1_counts <- pop_table[place1_index, "n_house"]
  place2_counts <- pop_table[place2_index, "n_house"]
  
  pop_table[place1_index, "n_house"] <- place1_counts + place2_counts
  pop_table <- pop_table[-place2_index, ]
  
  return(pop_table)
}

#' Remove a row from the pop_table 
#' 
#' @param pop_table dataframe to update
#' @param place character of place_ids to remove
#' 
#' @return pop_table updated dataframe with the 
#' desired place removed 
remove_count <- function(pop_table, place) {
  place_index <- which(pop_table$place_id == place)
  pop_table <- pop_table[-place_index, ]
  return(pop_table)
}
# 