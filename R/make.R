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
make_data <- function(pop_table, shapefile, pums_h, pums_p, schools, parallel = FALSE, 
                      sampling_type = "uniform", output_dir = "/home/lee/south_dakota/", 
                      convert_count) {
  
  start_time <- Sys.time()
  
  # Call the make_place function for each place in our pop_table. Either 
  # run this in parallel of not (usually I don't for debugging purposes)
  num_places <- nrow(pop_table) 
  
  if (parallel == FALSE) {
    
    for (place in 1:num_places) { 
      msg <- paste0("Generating place: ", place, " out of ", num_places)
      node_name <- paste0("Node: ", Sys.info()[['nodename']])      
      session_id <- paste0("R Session ID: ", Sys.getpid())
      print(msg)
      print(node_name)
      print(session_id)
      
      make_place(place, pop_table, shapefile, pums_h, pums_p, schools, 
                 sampling_type, output_dir, convert_count) 
    }    
  } else {
    # Set up the worker cores and export all of the necessary 
    # data needed to call the make_place function 
    num_workers <- parallel::detectCores()
    cluster <- parallel::makeCluster(num_workers, outfile = "")
    
    export_objects <- c("num_places", "make_place", "pop_table", "shapefile", "pums_h", 
                        "pums_p", "schools","sampling_type", "output_dir", "convert_count", 
                        "people_to_households", "sample_households", "sample_locations", 
                        "sample_people", "write_data", "people_to_households")
    
    parallel::clusterExport(cl = cluster, varlist = export_objects, envir = environment())    
    doParallel::registerDoParallel(cluster)    
    
    foreach(place = 1:num_places) %dopar% {
      
      # Print out relevant information pertaining to the job 
      msg <- paste0("Generating place: ", place, " out of ", num_places)
      node_name <- paste0("Node: ", Sys.info()[['nodename']])      
      session_id <- paste0("R Session ID: ", Sys.getpid())
      print(msg)
      print(node_name)
      print(session_id)
        
      make_place(place, pop_table, shapefile, pums_h, pums_p, schools,
                 sampling_type, output_dir, convert_count)
    }
    
    parallel::stopCluster(cluster)
  }
  
  # Print the diagnostics and summaries of the entire place 
  overall_time <- difftime(Sys.time(), start_time,units = "secs")
  overall_time <- round(overall_time, digits = 2)
  
  total_hh <- sum(pop_table$n_house)
  hh_statement <- paste0("Total Households: ", total_hh)
  time_statement <- paste0("Total Time: ", overall_time)
  
  print(hh_statement)
  print(time_statement)
  return(overall_time)
}

#' Create microdata for individual places 
#' 
#' @param index numeric with the pop_table row to generate
#' @param pop_table dataframe with columns corresponding to 
#' which places need populations, and how many samples to take 
#' @param shapefile sp class object used for assigning households to 
#' particular locations  
#' @param pums_h dataframe with microdata corresponding to housegolds 
#' @param pums_p dataframe with microdata corresponding to people 
#' @param sampling_type character vector indicating the type of sample to use for 
#' generating microdata. Right now the only value here is "uniform"
#' @param output dir character vector containing the location to save the
#' @param make_plots boolean indicating whether spew  makes plots of the synthetic households
#' synthetic population  
#' @return synthetic population .csv file for both household and person 
#' level data  
make_place <- function(index, pop_table, shapefile, pums_h, pums_p, schools,
                       sampling_type, output_dir, convert_count) {
  
  start_time <- Sys.time()
  
  # Make sure there are people living in this particular 
  # place. If not, skip!
  if (pop_table[index, "n_house"] == 0) {
    print("Place has 0 Households!")
    return(TRUE)
  }
  
  # Obtain the specific parameters for this run of make 
  n_house <- pop_table[index, "n_house"]
  puma_id <- pop_table[index, "puma_id"]
  place_id <- pop_table[index, "place_id"]
  
  # Convert people counts to household counts 
  if (convert_count == TRUE) {
    hh_sizes <- pums_h$PERSONS
    
    if (is.null(hh_sizes)) {
      hh_sizes <- nrow(pums_p) / nrow(pums_h)
    }
    
    n_house <- people_to_households(hh_sizes, n_house)
  }
  
  # Sample n indices from the household pums 
  households <- sample_households(n_house, pums_h, puma_id)
  sampled_households <- pums_h[households, ]
  
  # Attach locations to the sample households 
  locations <- sample_locations(place_id = place_id, n_house = n_house, 
                                shapefile = shapefile)
  sampled_households$longitude <- locations@coords[, 1]
  sampled_households$latitude <- locations@coords[, 2]
  
  # Add a synthetic serial ID and place ID 
  # to the sampled households 
  sampled_households$SYNTHETIC_SERIAL <- 1:nrow(sampled_households)
  stopifnot(!any(duplicated(sampled_households$SYNTHETIC_SERIAL)))
  
  sampled_households$place_id <- place_id
  sampled_households$puma_id <- puma_id
  
  # Attach people to the sampled households and make 
  # sure to include both the place and puma id
  sampled_people <- sample_people(sampled_households, pums_p)
  sampled_people$place_id <- place_id
  sampled_people$puma_id <- puma_id
  
  # Assign schools to people if the data exists 
  if (!is.null(schools)) {
    school_ids <- assign_schools(sampled_people, schools)
    sampled_people$school_id <- school_ids
    stopifnot("school_id" %in% names(sampled_people))
  }
  
  # Write the synthetic populations as CSV's
  write_data(df = sampled_households, place_id = place_id, 
             puma_id = puma_id, type = "household", 
             output_dir = output_dir)
  write_data(df = sampled_people, place_id = place_id, 
             puma_id = puma_id, type = "people", 
             output_dir = output_dir)

  # Print out diagnostics/summaries of this place 
  overall_time <- difftime(Sys.time(), start_time, units = "secs")
  overall_time <- round(overall_time, digits = 2)
  
  total_households <- nrow(sampled_households)
  total_people <- nrow(sampled_people)
  
  hh_statement <- paste0("Households: ", total_households)
  people_statement <- paste0("People: ", total_people)
  time_statement <- paste0("Time: ", overall_time)
  
  print(paste0("Place: ", place_id))
  print(paste0("Puma: ", puma_id))
  print(hh_statement)
  print(people_statement)
  print(time_statement)
  return(overall_time)
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
      if (!is.na(puma_id)) {
        if (!(puma_id %in% unique(pums_h$puma_id))) {
          sample_inds <- 1:nrow(pums_h)
        }
        else {
          sample_inds <- which(pums_h$puma_id == puma_id)
          stopifnot(length(sample_inds) < nrow(pums_h))
        }
      } else {
          sample_inds <- 1:nrow(pums_h)
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
  
  # Contingency if either the shapefile has duplicate 
  # regions, or if the Polygon has multiple polygons. In 
  # both cases, subset the first and remove the second
  if (length(region) > 1) {
    region <- region[1]
    locs <- sp::spsample(slots[[region]], n = n_house, offset = c(0, 0), 
                         type = "random", iter = 50)
  } else if (length(slots[[region]]@Polygons) > 1) {
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
#' @param puma_id numeric indicating the puma this synthetic population belongs to 
#' @param type character vector with the type, either "household" or "people"
#' @param output_dir character containing the directory in which we want to 
#' write the final csv's 
#' @return data indicating the indices of people to sample 
write_data <- function(df, place_id, puma_id, type, output_dir) {
  
  # Make a sub-directory for the puma_id if it exists 
  if (!(is.na(puma_id))) {
    directory <- paste0(output_dir, puma_id, "/")  
    
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)          
    }
    
    filename <- paste0(directory, type, "_", as.character(place_id), ".csv")
    write.table(df, filename, sep = ",", row.names = FALSE, qmethod = "double")
    
  } else {
    filename <- paste0(output_dir, type, "_", as.character(place_id), ".csv")
    write.table(df, filename, sep = ",", row.names = FALSE, qmethod = "double")
  }  
  return(TRUE)
}

#' Convert a population count to household count 
#' 
#' @param hh_sizes numeric vector with the household 
#' sizes for this particular sampling region  
#' @param n_people numeric indicating the number 
#' of people in this specific region 
#' @return number of households 
people_to_households <- function(hh_sizes, n_people) {
  hh_avg <- mean(hh_sizes)
  num_households <- n_people / hh_avg
  return(num_households)
}
