#' Create microdata using formatted data 
#' 
#' @param pop_table dataframe with columns corresponding to 
#' which places need populations, and how many samples to take 
#' @param shapefile sp class object used for assigning households to 
#' particular locations  
#' @param dataframe with microdata corresponding to housegolds 
#' @param dataframe with microdata corresponding to people 
#' @param logical indicating whether or not we will generate our 
#' synthetic populations in parallel
#' @param character vector indicating the type of sample to use for 
#' generating microdata 
#' @return logical specifying whether the microdata was generated 
#' successfully 
#' @examples
#' make_data(sd_data$pop_table, sd_data$shapefiles, sd_data$pums$pums_h, sd_data$pums$pums_p)
make_data <- function(pop_table, shapefile, pums_h, pums_p, parallel = FALSE, 
                      sampling_type = "uniform", output_dir = "/home/lee/south_dakota/") {
  
  num_places <- nrow(pop_table) 
  for (place in 1:num_places) {

    # Sample n indices from the household pums 
    households <- sample_households(pop_table[place, "n_house"], 
                                    pums_h, pop_table[place, "puma_id"])
    sampled_households <- pums_h[households, ]
    
    # Attach locations to the sample households 
    locations <- sample_locations(place_id = pop_table[place, "place_id"], 
                                  n_house = pop_table[place, "n_house"], 
                                  shapefile = shapefile)
    sampled_households$longitude <- locations@coords[, 1]
    sampled_households$latitude <- locations@coords[, 2]
    
    
    # Attach people to the sampled households 
    sampled_people <- sample_people(sampled_households, pums_p)
    
    # Output the synthetic population's as a csv
    write_data(df = sampled_households, place_id = pop_table[place, "place_id"], 
               type = "household", output_dir = output_dir)
    write_data(df = sampled_people, place_id = pop_table[place, "place_id"], 
               type = "people", output_dir = output_dir)
    
  }
}


#' Create microdata using formatted data 
#' 
#' @param n_house numeric indicating the number of households to sample 
#' @param pums_h dataframe of the households we are sampling from 
#' @param puma_id vector indicating which specific puma in PUMS we are sampling 
#' from, if any 
#' @param sampling_type character indicating the method for sampling 
#' @return numeric with the indicies of the household PUMS to sample 
sample_households <- function(n_house, pums_h, puma_id = NULL,
                              sampling_type = "uniform") {
  
  if (sampling_type == "uniform") {
    
    # Subset to a specific PUMA if we have data to do this 
    if (!is.null(puma_id)) {
      sample_inds <- which(pums_h$puma_id == puma_id)
    } else {
      sample_inds <- 1:nrow(puma_id)
    }
    
    households <- sample(sample_inds, n_house, replace = TRUE)
    return(households)
  }
}

#' Sample from a particular polygon shapefile 
#' 
#' @param place_id numeric specifiying the ID of the region we are 
#' subsampling  
#' @param nhouse numeric indicating the number of households
#' @param shapefile sp class with all of the locations for each place id
#' @return SpatialPoints object with coordinates for the n households
sample_locations <- function(place_id, n_house, shapefile) {
  slots <- methods::slot(shapefile, "polygons")
  region <- which(shapefile$place_id == place_id)
  
  # Contingency if the Polygons object has polygons, at 
  # least one of which has holes 
  if (length(slots[[region]]@Polygons) > 1) {
    first_polygon <- slots[[region]]@Polygons[[1]]
    locs <- sp::spsample(first_polygon, n = n_house, offset = c(0, 0), 
                         type = "random", iter = 50)
  } else {
    locs <- sp::spsample(slots[[region]], n = n_house, offset = c(0, 0), 
                         type = "random", iter = 50)    
  }
  return(locs)
}

#' Sample from the individual person PUMS data frame 
#' 
#' @param household_pums dataframe with the sampled houehold PUMS 
#' @param pums_p dataframe of the individual microdata 
#' @return data indicating the indices of people to sample 
sample_people <- function(household_pums, pums_p) {
  people <- plyr::join(household_pums, pums_p, type = "left", by = "SERIALNO")
  return(people)
}

#' Output our final synthetic populations as csv's 
#' 
#' @param df dataframe with the final synthetic population 
#' @param place_id numeric indicating the name of the particular region samples
#' @param type character vector with the type, either "household" or "people"
#' @param output_dir character containing the directory in which we want to 
#' write the final csv's 
#' @return data indicating the indices of people to sample 
write_data <- function(df, place_id, type, output_dir) {
  filename <- paste0(output_dir, type, "_", as.character(place_id), ".csv")
  write.csv(df, filename)
}
