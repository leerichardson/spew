#' Wrapper function to read, format, and generate SPEW synthetic ecosystems 
#' 
#' @param base_dir character specifying ecosystem directory
#' @param folders list specifying sub-directories for each data-source 
#' @param data-group character, either "US", "ipums", or "none"
#' @param parallel_type Indicating to run sequentially or parallel. If parallel, 
#' the back-end is specified, either "MPI", "SOCK", or "MC"
#' @param sampling_method character of sampling method. Default: "uniform", can also 
#' be "ipf" if appropriate marginal data is included
#' @param locations_method character vector indicating the type of location 
#' sampling to use. Default to "uniform". can also be "roads" if appropriate line 
#' shapefiles are included. 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param vars list with two elements: household and person. This specifies 
#' which variables to include in the corresponding household and person PUMS data-set
#' @export
#' @return logical indicating whether or not this run of spew ended successfully 
call_spew <- function(base_dir, folders = NULL, data_group = "US", parallel_type = "SEQ",
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
       parallel_type = parallel_type,  sampling_method = sampling_method, 
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
#' @param pop_table dataframe with rows corresponding to places which 
#' need to have a population generated. Columns also required are "puma_id"
#' and "n_house". 
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
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param parallel_type Indicating to run sequentially or parallel. Default: "SEQ". 
#' If parallel, the back-end is specified, either "MPI", "SOCK", or "MC"
#' @param sampling_method character vector indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param outfile_loc Defaults to "", so we print out the parallel run information. 
#' Only set to "/dev/null" for internal testing putposes. 
#' 
#' @export
#' @return logical indicating whether or not this run of spew ended successfully 
spew <- function(base_dir, pop_table, shapefile, pums_h, pums_p, schools, 
                 workplaces, marginals, convert_count, parallel_type = "SEQ", 
                 sampling_method = "uniform", locations_method = "uniform", 
                 outfile_loc = "") {
  location_start_time <- Sys.time()
  
  # Update, create output directory based on the base directory 
  output_dir <- file.path(base_dir, "output")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)          
  }
  
  # Write out the Popualation Table 
  write_pop_table(pop_table, output_dir)
  
  # Write out the Environments  
  env_dir <- file.path(output_dir, "environments")
  if (!dir.exists(env_dir)) {
    dir.create(env_dir, recursive = TRUE)          
  }
  
  if (!is.null(schools)) {    
    print("Writing Schools!")
    write_schools(schools, env_dir)
  }
  
  if (!is.null(workplaces)) {
    print("Writing Workplaces!")
    write_workplaces(workplaces, env_dir)
  }
  
  # Call either the sequential, or parallel version of the SPEW algorithm 
  num_places <- nrow(pop_table)
  if (parallel_type == "SEQ") {
    region_list <- spew_seq(num_places, pop_table, shapefile, pums_h, pums_p, 
                            schools, workplaces, marginals, sampling_method, 
                            locations_method, convert_count, output_dir)
    
  } else if (parallel_type == "MPI") {
    region_list <- spew_mpi(num_places, pop_table, shapefile, pums_h, pums_p, 
                            schools, workplaces, marginals, sampling_method, 
                            locations_method, convert_count, output_dir)
    
  } else if (parallel_type == "SOCK") {
    region_list <- spew_sock(num_places, pop_table, shapefile, pums_h, pums_p, 
                             schools, workplaces, marginals, sampling_method, 
                             locations_method, convert_count, output_dir, 
                             outfile_loc)
    
  } else if (parallel_type == "MC") {
    region_list <- spew_mc(num_places, pop_table, shapefile, pums_h, pums_p, 
                           schools, workplaces, marginals, sampling_method, 
                           locations_method, convert_count, output_dir, 
                           outfile_loc)
    
  } else {
    stop("parallel_type must be SEQ, MPI, SOCK, or MC")
  }

  # Print the diagnostics and summaries of the entire place 
  print_region_list(region_list)
  location_time <- difftime(Sys.time(), location_start_time, units = "secs")
  location_time <- round(location_time, digits = 2)  
  location_time_statement <- paste0("Location runs in: ", location_time)
  print(location_time_statement)
  
  return(region_list)
}


#' Run SPEW Sequentially 
spew_seq <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                     schools, workplaces, marginals, sampling_method, 
                     locations_method, convert_count, output_dir) {  
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
  
  return(region_list)
}

#' Run SPEW in Parallel with an MPI backend 
spew_mpi <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                     schools, workplaces, marginals, sampling_method, 
                     locations_method, convert_count, output_dir) {
  # Set up the MPI workers
  num_workers <- mpi.universe.size()
  mpi.spawn.Rslaves(nslaves = num_workers)

  # Print out the processor information
  rk <- mpi.comm.rank(0)
  sz <- mpi.comm.size(0)
  name <- mpi.get.processor.name()
  cat("Hello, rank " , rk , " out of " , sz , " on " , name, "\n")

  # If we are on Olympus, make sure to switch to personal libraries
  # because the core olympus directories don't have our updated functions. 
  hostname <- system("hostname", intern = TRUE)
  print(paste0("Hostname: ", hostname))
  if (grep("olympus", hostname) == 1) {
    print("On Olympus, switching lib-paths")
    username <- system("whoami", intern = TRUE)
    print(paste0("Username: ", username))
    print(.libPaths())
    mpi.bcast.cmd(username <- system("whoami", intern = TRUE))    
    mpi.bcast.cmd(personal_lib <- grep(username, .libPaths()))
    mpi.bcast.cmd(print(.libPaths()[personal_lib]))
    mpi.bcast.cmd(print(personal_lib))
    mpi.bcast.cmd(.libPaths(new = c(.libPaths()[personal_lib])))
    mpi.bcast.cmd(print("New Lib-Paths"))
    mpi.bcast.cmd(print(.libPaths()))
  }

  # Send the relevant data objects/packes to workers 
  mpi.bcast.cmd(library(plyr))
  mpi.bcast.cmd(library(methods))
  mpi.bcast.cmd(library(sp))
  mpi.bcast.cmd(library(rgeos))
  mpi.bcast.cmd(library(data.table))
  mpi.bcast.cmd(library(bit64))
  mpi.bcast.cmd(library(mipfp))
  
  # Export functions to all of the workers ------
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
                      "get_cat_dists", "remove_excess")
  
  for (obj in export_objects) {
    cat("Sending To Workers: ", obj, fill = TRUE)
    do.call("mpi.bcast.Robj2slave" , list (obj = as.name(obj)))
  }
  
  # Call SPEW for each one of the places 
  region_list <- mpi.applyLB(X = 1:num_places, 
                            FUN = spew_place, 
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
  
  # Close the connections and MPI
  mpi.close.Rslaves()
  mpi.exit()
  
  return(region_list)
}

#' Run SPEW in Parallel with a SOCK backend 
spew_sock <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                      schools, workplaces, marginals, sampling_method, 
                      locations_method, convert_count, output_dir, outfile_loc) {
  # Set up a SOCK cluster 
  num_workers <- min(num_places, parallel::detectCores(), 64) 
  cluster <- makeCluster(num_workers, type = "SOCK", outfile = outfile_loc)
  doParallel::registerDoParallel(cluster)
  
  # Export objects to the SOCK cluster  
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
                      "get_cat_dists", "remove_excess")
  parallel::clusterExport(cl = cluster, varlist = export_objects, envir = environment())  
  
  # Run for-each to generate each place on a separate core 
  region_list <- foreach(place = 1:num_places, 
                         .packages = c("plyr", "methods", "sp", "rgeos", 
                                       "data.table", "bit64", "mipfp"),
                         .errorhandling = 'pass', 
                         .export = export_objects) %dopar% {
                           print(paste0("Region ", place, " out of ", num_places))
                           times <- spew_place(index = place, 
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
                           print(paste0("Finished ", pop_table[place, "place_id"]))
                           times
                         }
  stopCluster(cluster)
  
  return(region_list)
}

#' Run SPEW in Parallel with a Multicore backend 
spew_mc <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                    schools, workplaces, marginals, sampling_method, 
                    locations_method, convert_count, output_dir, outfile_loc) {  
  # Set up the Multi-core Cluster
  num_workers <- min(num_places, parallel::detectCores(), 64)   
  doMC::registerDoMC(num_workers)
  
  # Run for-each to generate each place on a separate core 
  region_list <- foreach(place = 1:num_places, 
                         .packages = c("plyr", "methods", "sp", "rgeos", 
                                       "data.table", "bit64", "mipfp"),
                         .errorhandling = 'pass') %dopar% {
                           print(paste0("Region ", place, " out of ", num_places))
                           times <- spew_place(index = place, 
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
                           print(paste0("Finished ", pop_table[place, "place_id"]))
                           times
                         }  
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
  
  # To avoid data-frames larger than 5 Million, generate large 
  # places incrementally, appending rows onto each-other 
  append <- FALSE
  max_size <- 3000000
  iter <- 0
  number_iters <- floor(n_house / max_size)
  if (number_iters == 0) {
    iter_vec <- n_house
  } else {
    remainder <- n_house %% max_size 
    iter_vec <- c(rep(max_size, number_iters), remainder)
  }
  
  for (n_house in iter_vec) {
    # If this is the second increment of this place, append the 
    # final households 
    iter <- iter + 1
    if (iter == 2) {
      append <- TRUE
    }
    
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
    rm(locations); gc()
    
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
               output_dir = output_dir, append = append)          
    write_data(df = sampled_people, place_id = place_id, 
               puma_id = puma_id, type = "people", 
               output_dir = output_dir, append = append)    
  }
  
  # Collect diagnostic and summary information on this particular 
  # job and return this to the make_data function for analysis 
  place_time <- difftime(Sys.time(), place_start_time, units = "secs")
  place_time <- as.numeric(round(place_time, digits = 2))
  place_time_statement <- paste0("Time: ", place_time)
  
  hh_statement <- paste0("Households: ", n_house)
  people_statement <- paste0("People: ", nrow(sampled_people))  
  school_statement <- paste0("Schools: ", school_time)
  workplace_statement <- paste0("Workplaces: ", workplace_time)  
  place_statement <- paste0("Place: ", index) 
  total_place_statement <- paste0("Total Places: ", nrow(pop_table))
  place_name_statement <- paste0("Place Name: ", place_id)
  puma_statement <- paste0("Puma: ", puma_id)
  
  # Remove largest objects, then to Garbage collection
  rm(sampled_people); rm(sampled_households); gc()
  
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
write_data <- function(df, place_id, puma_id, type, output_dir, append = FALSE) {
  # Create the output directory for this PUMA 
  directory <- file.path(output_dir, paste0("output_", puma_id), "eco")  
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)          
  }
  
  filename <- file.path(directory, paste0(type, "_", as.character(place_id), ".csv"))
  filename <- remove_excess(filename)
  
  data.table::fwrite(df, filename, sep = ",", qmethod = "double", append = append)
  
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
  pop_table$place_id <- remove_excess(pop_table$place_id)
  filename <- file.path(output_dir, "final_pop_table.csv")
  filename <- remove_excess(filename)
  write.csv(pop_table, filename)
  return(TRUE)
}

#' Write school environment 
#' 
#' @param schools school data-object 
#' @param env_dir filepath to write out the environment 
#' 
#' @return logical TRUE if completed successfully
write_schools <- function(schools, env_dir) {
  # Write out the public, then private school CSV's!
  public_df <- schools[["public"]]
  public_df$School <- remove_excess(name = public_df$School)
  public_filename <- file.path(env_dir, "public_schools.csv")  
  write.csv(public_df, public_filename)
  
  private_df <- schools[["private"]]
  private_df$School <- remove_excess(name = private_df$School)
  private_filename <- file.path(env_dir, "private_schools.csv")  
  write.csv(private_df, private_filename)
  
  return(TRUE)
}

#' Write workplaces environment 
#' 
#' @param workplaces school data-object 
#' @param env_dir filepath to write out the environment 
#' 
#' @return logical TRUE if completed successfully
write_workplaces <- function(workplaces, env_dir) {
  workplaces_filename <- file.path(env_dir, "workplaces.csv")  
  write.csv(workplaces, workplaces_filename)
  
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
  # remainder after this split for the final partition
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
