#' Create synthetic microdata 
#' 
#' @param pop_table dataframe with columns corresponding to 
#' which places need populations, and how many samples to take 
#' @param shapefile sp class object used for assigning households to 
#' particular locations  
#' @param pums_h dataframe with microdata corresponding to housegolds 
#' @param pums_p dataframe with microdata corresponding to people 
#' @param parallel logical indicating whether or not we will generate our 
#' synthetic populations in parallel
#' @param sampling_type character vector indicating the type oof sampling used. 
#' Default's to "uniform"
#' @param output_dir character vector specifying where to write the synthetic microdata 
#' @return logical specifying whether the microdata was generated 
#' successfully 
#' @examples
#' make_data(sd_data$pop_table, sd_data$shapefiles, sd_data$pums$pums_h, sd_data$pums$pums_p)
make_data <- function(pop_table, shapefile, pums_h, pums_p, parallel = FALSE, 
                      sampling_type = "uniform", output_dir = "/home/lee/south_dakota/") {
  
  
  # Call the make_place function for each place in our pop_table. Either 
  # run this in parallel of not (usually I don't for debugging purposes)
  num_places <- nrow(pop_table) 
  if (parallel == FALSE) {
    
    for (place in 1:num_places) { 
      msg <- paste0("Generating place: ", place, " out of ", num_places)
      print(msg)
      
      make_place(place, pop_table, shapefile, pums_h, pums_p, sampling_type, output_dir) 
    }    
  } else {
    # Set up the worker cores and export all of the necessary 
    # data needed to call the make_place function 
    num_workers <- parallel::detectCores()
    cluster <- parallel::makeCluster(num_workers)
    doParallel::registerDoParallel(num_workers)
    
    place_pops <- foreach(place = 1:num_places) %dopar% {
      
      msg <- paste0("Generating place: ", place, " out of ", num_places)
      print(msg)
      
      make_place(place, pop_table, shapefile, pums_h, pums_p, sampling_type, output_dir)
    }
    return(place_pops)
  }
}


#' Create microdata for individual places 
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
#' @return synthetic population .csv file for both household and person 
#' level data  
make_place <- function(index, pop_table, shapefile, pums_h, pums_p, 
                       sampling_type, output_dir) {
  
  # Sample n indices from the household pums 
  households <- sample_households(pop_table[index, "n_house"], 
                                  pums_h, pop_table[index, "puma_id"])
  sampled_households <- pums_h[households, ]
  
  # Attach locations to the sample households 
  locations <- sample_locations(place_id = pop_table[index, "place_id"], 
                                n_house = pop_table[index, "n_house"], 
                                shapefile = shapefile)
  sampled_households$longitude <- locations@coords[, 1]
  sampled_households$latitude <- locations@coords[, 2]
  
  
  # Attach people to the sampled households 
  sampled_people <- sample_people(sampled_households, pums_p)
  
  # Output the synthetic population's as a csv
  write_data(df = sampled_households, place_id = pop_table[index, "place_id"], 
             type = "household", output_dir = output_dir)
  write_data(df = sampled_people, place_id = pop_table[index, "place_id"], 
             type = "people", output_dir = output_dir)
  return(TRUE)
}


#' Sample appropriate indices from household PUMS 
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
