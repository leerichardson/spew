#' Wrapper function to read, format, and generate SPEW synthetic ecosystems 
#' 
#' @param base_dir character specifying ecosystem directory
#' @param folders list specifying sub-directories for each data-source 
#' @param data-group character, either "US", "ipums", or "none"
#' @param parallel logical whether or not to run in parallel
#' @param sampling_method character of sampling 
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals to househole counts
#' @param vars list with two components: household and person. This specifies 
#' which variables to include in the corresponding PUMS data-set  
#' @param make_plots logical indicating whether we should make maps of 
#' the synthetic households.  
#' @export
#' @return logical indicating whether or not this run of spew ended successfully 
call_spew <- function(base_dir, folders = NULL, data_group, parallel = TRUE, 
                          sampling_method = "uniform", locations_method = "uniform", 
                          convert_count = FALSE, vars = list(household = NA, person = NA)) {
  spew_start_time <- Sys.time()
  
  # Given directory, folders, vars, and data-group, read input data into a list 
  data_list <- read_data(base_dir = base_dir, folders = folders, data_group = data_group, vars)
  
  # Given the data list, make sure everything is formatted correctly 
  formatted_data <- format_data(data_list = data_list, data_group = data_group)
  
  # Call the SPEW algorithm on the formatted data 
  spew(base_dir = base_dir, pop_table = formatted_data$pop_table, 
       shapefile = formatted_data$shapefiles, pums_h = formatted_data$pums$pums_h, 
       pums_p = formatted_data$pums$pums_p, schools = formatted_data$schools, 
       workplaces = formatted_data$workplaces, marginals = formatted_data$marginals, 
       parallel = parallel,  sampling_method = sampling_method, 
       locations_method = locations_method, convert_count = convert_count)

  # Print out the overall run-time of SPEW!
  spew_time <- difftime(Sys.time(), spew_start_time, units = "secs")
  spew_time <- round(spew_time, digits = 2)
  spew_statement <- paste0("SPEW Runs in: ", spew_time)
  print(spew_statement)
  
  return(TRUE)
}

#' SPEW algorithm to generate synthetic ecosystems
#' 
#' @param base_dir character specifying ecosystem directory
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
#' @param marginals list of marginal population totals. Each element of the 
#' list contains the marginal totals of a separate variable 
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
spew <- function(base_dir, pop_table, shapefile, pums_h, pums_p, schools, 
                 workplaces, marginals, convert_count, parallel = FALSE, 
                 sampling_method = "uniform", locations_method = "uniform", 
                 outfile_loc = "") {
  location_start_time <- Sys.time()
  
  # Update, create output directory based on the base directory 
  output_dir <- file.path(base_dir, "output")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)          
  }
  
  # Write out the final, formatted population table  
  write_pop_table(pop_table, output_dir)
  
  # Call the make_place function for each place in our pop_table. Either 
  # run this in parallel of not (usually I don't for debugging purposes)
  num_places <- nrow(pop_table) 
  
  if (parallel == FALSE) {
    region_list <- vector(mode = "list", length = num_places)
    for (place in 1:num_places) {
      print(paste0("Region ", place, " out of ", num_places))
      region_list[[place]] <- spew_place(index = place, 
                                         pop_table = pop_table, 
                                         shapefile = shapefile, 
                                         pums_h = pums_h, 
                                         pums_p = pums_p, 
                                         schools = schools, 
                                         workplaces = workplaces, 
                                         marginals = marginals, 
                                         sampling_method = sampling_method, 
                                         locations_method = locations_method, 
                                         convert_count = convert_count, 
                                         output_dir = output_dir)
    }
    
  } else {
    # Set up the worker cores and export all of the necessary data
    num_workers <- min(num_places, parallel::detectCores(), 64)
    cluster <- parallel::makeCluster(num_workers, type = "SOCK", outfile = outfile_loc, useXDR = FALSE)
    export_objects <- c("spew_place", "people_to_households", "sample_households", 
                        "sample_locations", "sample_people", "write_data", 
                        "people_to_households", "assign_schools", "assign_schools_inner", 
                        "weight_dists", "get_dists", "haversine", "subset_schools", 
                        "assign_workplaces", "assign_workplaces_inner", "remove_holes", 
                        "sample_locations_uniform", "sample_locations_roads", 
                        "subset_shapes_roads", "samp_roads", "print_region_list", "read_roads", 
                        "sample_uniform", "sample_ipf", "subset_pums", "align_pums", 
                        "fill_cont_table", "update_freqs", "get_targets", "sample_with_cont", 
                        "samp_ipf_inds", "assign_weights", "calc_dists", "get_ord_dists", 
                        "get_cat_dists")
    parallel::clusterExport(cl = cluster, varlist = export_objects, envir = environment())  
    doParallel::registerDoParallel(cluster)
    
    # Send each place to an individual core 
    region_list <- foreach(place = 1:num_places, 
                           .packages = c("plyr", "methods", "sp", "rgeos", "data.table", "bit64", "mipfp"), 
                           .export = export_objects, 
                           .errorhandling = 'pass') %dopar% {
                             print(paste0("Region ", place, " out of ", num_places))
                             spew_place(index = place, 
                                        pop_table = pop_table, 
                                        shapefile = shapefile, 
                                        pums_h = pums_h, 
                                        pums_p = pums_p, 
                                        schools = schools, 
                                        workplaces = workplaces, 
                                        marginals = marginals, 
                                        sampling_method = sampling_method, 
                                        locations_method = locations_method, 
                                        convert_count = convert_count, 
                                        output_dir = output_dir)
                           }
    
    parallel::stopCluster(cluster)
  }
  
  # Print the diagnostics and summaries of the entire place 
  print_region_list(region_list)
  location_time <- difftime(Sys.time(), location_start_time, units = "secs")
  location_time <- round(location_time, digits = 2)  
  location_time_statement <- paste0("Location runs in: ", location_time)
  print(location_time_statement)
  
  return(region_list)
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
#' @param marginals list with elements corresponding to marginal totals 
#' of population variables 
#' @param sampling_method character vector indicating the type of sampling 
#' method to use, defaults to "uniform"
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads" 
#' @param output dir character vector containing the location to save the
#' @return synthetic population .csv file for both household and person 
#' level data
spew_place <- function(index, pop_table, shapefile, pums_h, pums_p, schools,
                       workplaces, marginals, output_dir, convert_count, sampling_method, 
                       locations_method) {
  # Start the clock on this specific place 
  place_start_time <- Sys.time()
  
  # Obtain the specific parameters for this run of make 
  n_house <- pop_table[index, "n_house"]
  puma_id <- pop_table[index, "puma_id"]
  place_id <- pop_table[index, "place_id"]
  
  # Skip if 0 people in a region
  if (pop_table[index, "n_house"] == 0 | n_house == 0) {
    print("Place has 0 Households!")
    return(TRUE)
  }
  
  # If there's a shapefile ID, extract from the pop table 
  if ("shapefile_id" %in% names(pop_table)) {
    shapefile_id <- pop_table[index, "shapefile_id"]
  } else {
    shapefile_id <- NULL
  }
  
  # Convert people counts to household counts 
  if (convert_count == TRUE) {
    hh_sizes <- pums_h$PERSONS
    if (is.null(hh_sizes)) {
      hh_sizes <- nrow(pums_p) / nrow(pums_h)
    }
    n_house <- floor(people_to_households(hh_sizes, n_house))
  }
  
  # Households --------------- 
  sampled_households <- sample_households(method = sampling_method, 
                                          n_house = n_house, 
                                          pums_h = pums_h, 
                                          pums_p = pums_p, 
                                          marginals = marginals,
                                          puma_id = puma_id, 
                                          place_id = place_id)
  
  # Locations ----------------
  locations <- sample_locations(method = locations_method, 
                                place_id = place_id,
                                n_house = n_house, 
                                shapefile = shapefile, 
                                noise = .0002, 
                                shapefile_id)
  
  sampled_households$longitude <- locations@coords[, 1]
  sampled_households$latitude <- locations@coords[, 2]
  
  # People ----------------
  sampled_people <- sample_people(method = sampling_method, 
                                  household_pums = sampled_households, 
                                  pums_p = pums_p, 
                                  puma_id = puma_id, 
                                  place_id = place_id)
  
  # Schools --------------
  school_time <- 0
  if (!is.null(schools)) {
    school_start_time <- Sys.time()
    
    school_ids <- assign_schools(sampled_people, schools)
    sampled_people$school_id <- school_ids
    stopifnot("school_id" %in% names(sampled_people))
    
    school_time <- difftime(Sys.time(), school_start_time, units = "secs")
    school_time <- round(school_time, digits = 2)
  }
  
  # Workplaces -------------- 
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
  # Create the output directory for this PUMA 
  directory <- file.path(output_dir, paste0("output_", puma_id), "eco")  
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)          
  }
  
  filename <- file.path(directory, paste0(type, "_", as.character(place_id), ".csv"))
  data.table::fwrite(df, filename, sep = ",", qmethod = "double")
  
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
  filename <- file.path(output_dir, "final_pop_table.csv")
  write.csv(pop_table, filename)
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

#' Partition the poptable into chunks 
#' 
#' @param total size numeric indicating the size of 
#' the pop table 
#' @param partition_size indicating the size of chunks 
#' to partition the pop table into 
#' @return 
partition_pt <- function(total_size, partition_size) {
  # Get the total number of partitions, and the the 
  # remainder after this splitfor the final partition
  number_partitions <- floor(total_size / partition_size)
  final_partition_size <- total_size %% partition_size
  partitions <- seq(1, (number_partitions * (partition_size)) + 1, by = partition_size)
  
  # If it's not ab equal split, add the final remainder. If
  # it is, then make sure the final element is the same 
  # as the total size. 
  n <- length(partitions)
  if (final_partition_size != 0) {
    partitions <- c(partitions, partitions[n] + final_partition_size - 1)
  } else {
    partitions[n] <- total_size
  }
  stopifnot(partitions[length(partitions)] == total_size)
  
  return(partitions)
}
