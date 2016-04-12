#' Create synthetic microdata 
#' 
#' @param pop_table dataframe with columns corresponding to 
#' which places need populations, and how many samples to take 
#' @param shapefile sp class object used for assigning households to 
#' particular locations  
#' @param pums_h dataframe with microdata corresponding to housegolds 
#' @param pums_p dataframe with microdata corresponding to people
#' @param schools list with names "public" and "private" with a 
#' dataframe of schools corresponding to public or private, respectively
#' @param workplaces dataframe of workplaces with a workplace_id column, 
#' employees column, and stcotr column
#' @param parallel logical indicating whether or not we will generate our 
#' synthetic populations in parallel
#' @param sampling_method character vector indicating the type of sampling 
#' method to use, defaults to "uniform"
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads" 
#' @param output_dir character vector specifying where to write the synthetic microdata 
#' @return logical specifying whether the microdata was generated 
#' successfully 
#' @examples
#' make_data(sd_data$pop_table, sd_data$shapefiles, sd_data$pums$pums_h, sd_data$pums$pums_p)
make_data <- function(pop_table, shapefile, pums_h, pums_p, schools, workplaces, 
                      convert_count, output_dir, parallel = FALSE, 
                      sampling_method = "uniform", locations_method = "uniform") {
  
  location_start_time <- Sys.time()
  
  # Write out the final, formatted population table  
  write_pop_table(pop_table, output_dir)
  
  # Call the make_place function for each place in our pop_table. Either 
  # run this in parallel of not (usually I don't for debugging purposes)
  num_places <- nrow(pop_table) 
  
  if (parallel == FALSE) {
    region_list <- vector(mode = "list", length = num_places)
    for (place in 1:num_places) { 
      print(paste0("Region ", place, " out of ", num_places))
      region_list[[place]] <- make_place(index = place, pop_table = pop_table, shapefile = shapefile, 
                                         pums_h = pums_h, pums_p = pums_p, schools = schools, 
                                         workplaces = workplaces, sampling_method = sampling_method, 
                                         locations_method = locations_method, output_dir = output_dir, 
                                         convert_count = convert_count) 
    }

  } else {
    # Set up the worker cores and export all of the necessary 
    # data needed to call the make_place function 
    num_workers <- parallel::detectCores()
    cluster <- parallel::makeCluster(num_workers, outfile = "")
    
    export_objects <- c("make_place", "people_to_households", "sample_households", 
                        "sample_locations", "sample_people", "write_data", 
                        "people_to_households", "assign_schools", "assign_schools_inner", 
                        "weight_dists", "get_dists", "haversine", "subset_schools", 
                        "assign_workplaces", "assign_workplaces_inner", "remove_holes", 
                        "sample_locations_uniform", "sample_locations_from_roads", "subset_shapes_roads", 
                        "samp_roads")
    
    parallel::clusterExport(cl = cluster, varlist = export_objects, envir = environment())    
    doSNOW::registerDoSNOW(cluster)
  
    # Run each region in parallel     
    region_list <- foreach(place = 1:num_places, .packages = c("plyr"), .export = export_objects) %dopar% {
                    print(paste0("Region ", place, " out of ", num_places))
                      make_place(index = place, pop_table = pop_table, shapefile = shapefile, 
                                   pums_h = pums_h, pums_p = pums_p, schools = schools, 
                                   workplaces = workplaces, sampling_method = sampling_method, 
                                   locations_method = locations_method, output_dir = output_dir, 
                                   convert_count = convert_count)    
                    }
    parallel::stopCluster(cluster)
  }

  # Print the diagnostics and summaries of the entire place 
  print_region_list(region_list)
  
  location_time <- difftime(Sys.time(), location_start_time, units = "secs")
  location_time <- round(location_time, digits = 2)  
  location_time_statement <- paste0("Location runs in: ", location_time)
  print(location_time_statement)
  
  return(TRUE)
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
#' @param schools dataframe with data corresponding to available schools 
#' @param workplaces dataframe with data corresponding to available workplaces 
#' @param sampling_method character vector indicating the type of sampling 
#' method to use, defaults to "uniform"
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads" 
#' @param output dir character vector containing the location to save the
#' @return synthetic population .csv file for both household and person 
#' level data  
make_place <- function(index, pop_table, shapefile, pums_h, pums_p, schools,
                       workplaces, output_dir, convert_count, sampling_method, 
                       locations_method) {
  # Start the clock on this specific place 
  place_start_time <- Sys.time()

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
  households <- sample_households(method = sampling_method, 
                                  n_house = n_house, 
                                  pums_h = pums_h, 
                                  puma_id = puma_id)
  sampled_households <- pums_h[households, ]

  # Add ID, place, and puma columns to the synthetic household 
  sampled_households$SYNTHETIC_HID <- paste0(place_id, "-", 1:nrow(sampled_households))
  stopifnot(!any(duplicated(sampled_households$SYNTHETIC_SERIAL)))
  sampled_households$place_id <- place_id
  sampled_households$puma_id <- puma_id
    
  # Attach locations to the sample households
  locations <- sample_locations(method = locations_method, 
                                place_id = place_id,
                                n_house = n_house, 
                                shapefile = shapefile, 
                                noise = .0002)
  sampled_households$longitude <- locations@coords[, 1]
  sampled_households$latitude <- locations@coords[, 2]
  
  # Attach people to the sampled households and make 
  # sure to include both the place and puma id
  sampled_people <- sample_people(method = sampling_method, 
                                  household_pums = sampled_households, 
                                  pums_p = pums_p)
  
  sampled_people$place_id <- place_id
  sampled_people$puma_id <- puma_id
  sampled_people$SYNTHETIC_PID <- paste0(sampled_people$SYNTHETIC_HID, "-", 1:nrow(sampled_people))
  stopifnot(!any(duplicated(sampled_people$SYNTHETIC_PID)))
  
  # Assign schools to people if the data exists 
  school_time <- 0
  if (!is.null(schools)) {
    school_start_time <- Sys.time()
    
    school_ids <- assign_schools(sampled_people, schools)
    sampled_people$school_id <- school_ids
    stopifnot("school_id" %in% names(sampled_people))
    
    school_time <- difftime(Sys.time(), school_start_time, units = "secs")
    school_time <- round(school_time, digits = 2)
  }

  # Assign workplaces to people if the data exists 
  workplace_time <- 0
  if (!is.null(workplaces)) {
    workplace_start_time <- Sys.time()
    
    workplace_ids <- assign_workplaces(sampled_people, workplaces)
    sampled_people$workplace_id <- workplace_ids
    stopifnot("workplace_id" %in% names(sampled_people))
    
    workplace_time <- difftime(Sys.time(), workplace_start_time, units = "secs")
    workplace_time <- round(school_time, digits = 2)
  }
  
  # Write the synthetic populations as CSV's
  write_data(df = sampled_households, place_id = place_id, 
             puma_id = puma_id, type = "household", 
             output_dir = output_dir)
  write_data(df = sampled_people, place_id = place_id, 
             puma_id = puma_id, type = "people", 
             output_dir = output_dir)

  # Collect diagnostic and summary information on this particular 
  # job and return this to the make_data function for analysis 
  place_time <- difftime(Sys.time(), place_start_time, units = "secs")
  place_time <- as.numeric(round(place_time, digits = 2))
  place_time_statement <- paste0("Time: ", place_time)
  
  hh_statement <- paste0("Households: ", nrow(sampled_households))
  people_statement <- paste0("People: ", nrow(sampled_people))
  school_statement <- paste0("Schools: ", school_time)
  workplace_statement <- paste0("Workplaces: ", workplace_time)  
  place_statement <- paste0("Place: ", index) 
  total_place_statement <- paste0("Total Places: ", nrow(pop_table))
  place_name_statement <- paste0("Place Name: ", place_id)
  puma_statement <- paste0("Puma: ", puma_id)
  
  return(list(place_name = place_name_statement,
              place_num = place_statement, 
              total_places = total_place_statement, 
              total_households = hh_statement, 
              total_people = people_statement, 
              place_time = place_time_statement, 
              schools = school_statement, 
              workplaces = workplace_statement, 
              puma_name = puma_statement))
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
  
    directory <- paste0(output_dir, "output_", puma_id, "/", "eco/")  
    
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)          
    }
    
    filename <- paste0(directory, type, "_", as.character(place_id), ".csv")
    write.table(df, filename, sep = ",", row.names = FALSE, qmethod = "double")
      
  return(TRUE)
}

#' Write out the final, formatted table 
#' 
#' @param pop_table the population table df 
#' @param output_dir character vector of the directory 
#' to write the population table 
#' 
#' @return logical TRUE if completed. As well as a written 
#' pop_table to the given output directory 
write_pop_table <- function(pop_table, output_dir) {
  filename <- paste0(output_dir, "final_pop_table.csv")
  write.table(pop_table, filename, sep = ",", row.names = FALSE, qmethod = "double")
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

#' Write out information on each region 
#' 
#'  @param region_list a list containing all of the 
#'  summary and diagnostic information for each  
print_region_list <- function(region_list) {
  # Loop through each element of a list and 
  # print our each element within that list 
  for (region in seq_along(region_list)) {
    for (diag in seq_along(region_list[[region]])) {
      diag_statement <- region_list[[region]][[diag]]
      print(diag_statement)
    }
  }
  
  return(TRUE)
}
