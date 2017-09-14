#' Wrapper for reading, formatting, and writing SPEW ecosystems 
#' 
#' Generates SPEW synthetic ecosystems on the Olympus Computing Cluster 
#' 
#' @param base_dir character specifying ecosystem directory
#' @param folders list specifying sub-directories for each data-source 
#' @param data_group character, either "US", "ipums", or "none"
#' @param run_type Whether to run sequentially in parallel. Default is "SEQ", for 
#' a sequential run. If parallel, back-end is either "MPI", "SOCK", or "MC"
#' @param output_type Default is "console" if we want to resulting population 
#' as an R variable. Alternative is "write", which is used on Olympus for writing 
#' out .csv files of the population
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param vars list with two elements: household and person. This specifies 
#' which variables to include in the corresponding household and person PUMS data-set
#' @param sampling_method character indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param outfile_loc Defaults to "", so we print out the parallel run information. 
#' Only set to "/dev/null" for internal testing putposes.
#' @param road_noise Noise added to households during road-based sampling 
#' @param timer logical indicating we want to time the run 
#' @param verbose logical indicating we want to print output during the run.  Default is FALSE.#' 
#' 
#' @export
#' 
#' @return logical indicating whether or not this run of spew ended successfully 
call_spew <- function(base_dir, folders = NULL, data_group = "US", run_type = "SEQ",
                      sampling_method = "uniform", locations_method = "uniform", output_type = "write", 
                      convert_count = FALSE, vars = list(household = NA, person = NA),
                      road_noise = .0002, outfile_loc = "", timer = TRUE, verbose = TRUE) {
  spew_start_time <- Sys.time()
    
  # Read and format data from olympus directories ---
  data_list <- read_data(base_dir = base_dir, folders = folders, data_group = data_group, vars)
  formatted_data <- format_data(data_list = data_list, data_group = data_group)

  # Call the SPEW algorithm on the formatted data ---
  spew(pop_table = formatted_data$pop_table, shapefile = formatted_data$shapefiles, 
       pums_h = formatted_data$pums$pums_h, pums_p = formatted_data$pums$pums_p,
       schools = formatted_data$schools, workplaces = formatted_data$workplaces, 
       marginals = formatted_data$marginals, output_type = output_type, base_dir = base_dir, 
       locations_method = locations_method, convert_count = convert_count,
       sampling_method = sampling_method, run_type = run_type, outfile_loc = outfile_loc, 
       road_noise = road_noise, timer = timer, verbose = verbose)

  # Print out the overall run-time of SPEW ---
  spew_time <- difftime(Sys.time(), spew_start_time, units = "secs")
  spew_time <- round(spew_time, digits = 2)
  spew_statement <- paste0("SPEW Runs in: ", spew_time)
  print(spew_statement)
  
  return(TRUE)
}

#' SPEW algorithm to generate synthetic ecosystems
#' 
#' @param pop_table dataframe where rows correspond to places where populations 
#' should be generated. Other requird columns are "n_house" and "puma_id"
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
#' @param output_type Default is "console" if we want to resulting population 
#' as an R variable. Alternative is "write", which is used on Olympus for writing 
#' out .csv files of the population
#' @param base_dir character specifying ecosystem directory write to. Only used if
#' output_type = "write"
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param run_type Whether to run sequentially in parallel. Default is "SEQ", for 
#' a sequential run. If parallel, back-end is either "MPI", "SOCK", or "MC"
#' @param sampling_method character indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param outfile_loc Defaults to "", so we print out the parallel run information. 
#' Only set to "/dev/null" for internal testing putposes.
#' @param road_noise Noise added to households during road-based sampling 
#' @param timer logical indicating we want to time the run 
#' @param verbose logical indicating we want to print output during the run.  Default is FALSE.
#' 
#' @export
#' 
#' @return logical indicating whether or not this run of spew ended successfully 
spew <- function(pop_table, shapefile, pums_h, pums_p, 
                 schools = NULL, workplaces = NULL, marginals = NULL, 
                 output_type = "console", base_dir = NULL, convert_count = FALSE, 
                 run_type = "SEQ", sampling_method = "uniform", locations_method = "uniform", 
                 outfile_loc = "", road_noise = .0002, timer = TRUE, verbose = FALSE) {
  if (timer == TRUE) { location_start_time <- Sys.time() }
  
  # Write out both the pop-table and environments ---
  output_dir <- NULL
  if (output_type == "write") {
    if (is.null(base_dir)) { 
      stop("If output_type = 'write', must specify output directory in base_dir") 
    }
  
    output_dir <- file.path(base_dir, "output")
    if (!is.null(output_dir)) { dir.create(output_dir, recursive = TRUE) }
    write_pop_table(pop_table, output_dir)
    
    env_dir <- file.path(output_dir, "environments")
    if (!dir.exists(env_dir)) { dir.create(env_dir, recursive = TRUE) }
    if (!is.null(schools)) { write_schools(schools, env_dir) }
    if (!is.null(workplaces)) { write_workplaces(workplaces, env_dir) }
  } 
  
  # Call one of the SPEW run-types ---

  # Set the objects to send to the workers if parallel back-end is SOCK or MPI 
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
                      "get_cat_dists", "remove_excess","sample_mm", "solve_mm_weights", 
                      "solve_mm_for_joint","solve_mm_for_var", "extrapolate_probs_to_pums",
                      "extrapolate_probs_to_pums_joint", "make_mm_obj", "impute_missing_vals")
  
  # Call either the sequential, or parallel version of the SPEW algorithm 
  num_places <- nrow(pop_table)
  if (run_type == "SEQ") {
    region_list <- spew_seq(num_places = num_places, pop_table = pop_table, shapefile = shapefile, 
                            pums_h = pums_h, pums_p = pums_p, schools = schools, 
                            workplaces = workplaces, marginals = marginals, output_type = output_type, 
                            output_dir = output_dir, convert_count = convert_count, 
                            sampling_method = sampling_method, locations_method = locations_method, 
                            outfile_loc = outfile_loc, export_objects = export_objects, 
                            road_noise = road_noise, timer = timer, verbose = verbose)

  } else if (run_type == "MPI") {
    region_list <- spew_mpi(num_places = num_places, pop_table = pop_table, shapefile = shapefile, 
                            pums_h = pums_h, pums_p = pums_p, schools = schools, 
                            workplaces = workplaces, marginals = marginals, output_type = output_type, 
                            output_dir = output_dir, convert_count = convert_count, 
                            sampling_method = sampling_method, locations_method = locations_method, 
                            outfile_loc = outfile_loc, export_objects = export_objects, 
                            road_noise = road_noise, timer = timer, verbose = verbose)
    
  } else if (run_type == "SOCK") {
    region_list <- spew_sock(num_places = num_places, pop_table = pop_table, shapefile = shapefile, 
                             pums_h = pums_h, pums_p = pums_p, schools = schools, 
                             workplaces = workplaces, marginals = marginals, output_type = output_type, 
                             output_dir = output_dir, convert_count = convert_count, 
                             sampling_method = sampling_method, locations_method = locations_method, 
                             outfile_loc = outfile_loc, export_objects = export_objects, 
                             road_noise = road_noise, timer = timer, verbose = verbose)
    
  } else if (run_type == "MC") {
    region_list <- spew_mc(num_places = num_places, pop_table = pop_table, shapefile = shapefile, 
                           pums_h = pums_h, pums_p = pums_p, schools = schools, 
                           workplaces = workplaces, marginals = marginals, output_type = output_type, 
                           output_dir = output_dir, convert_count = convert_count, 
                           sampling_method = sampling_method, locations_method = locations_method, 
                           outfile_loc = outfile_loc, export_objects = export_objects, 
                           road_noise = road_noise, timer = timer, verbose = verbose)
    
  } else {
    stop("run_type must be SEQ, MPI, SOCK, or MC")
  }

  # Print the diagnostics and summaries of the entire place
  if (verbose == TRUE) { print_region_list(region_list) }

  if (timer == TRUE) {
    location_time <- difftime(Sys.time(), location_start_time, units = "secs")
    location_time <- round(location_time, digits = 2)  
    location_time_statement <- paste0("Location runs in: ", location_time)
    print(location_time_statement)
  }  
  
  return(region_list)
}

#' Run SPEW Sequentially 
#' 
#' Internal function, only called by the main spew function
#' 
#' @param num_places The number of regions (place_ids) to be generated for a given location 
#' @param pop_table dataframe where rows correspond to places where populations 
#' should be generated. Other requird columns are "n_house" and "puma_id"
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
#' @param output_type Default is "console" if we want to resulting population 
#' as an R variable. Alternative is "write", which is used on Olympus for writing 
#' out .csv files of the population
#' @param output_dir directory to write output if output_type = "write", NULL otherwise 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param sampling_method character indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param outfile_loc Defaults to "", so we print out the parallel run information. 
#' Only set to "/dev/null" for internal testing putposes.
#' @param export_objects objects for exporting to parallel backends 
#' @param road_noise Noise added to households during road-based sampling 
#' @param timer logical indicating we want to time the run 
#' @param verbose logical indicating we want to print output during the run.  Default is FALSE.#' 
#' 
#' @return region_list with output_type for all num_place locations 
spew_seq <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                     schools, workplaces, marginals, output_type, output_dir, 
                     convert_count, sampling_method, locations_method, outfile_loc, 
                     export_objects, road_noise, timer, verbose) {
  
  region_list <- vector(mode = "list", length = num_places)
  for (place in 1:num_places) {
    if (verbose == TRUE) { print(paste0("Region ", place, " out of ", num_places)) }
    region_list[[place]] <- spew_place(index = place, 
                                       pop_table = pop_table, 
                                       shapefile = shapefile, 
                                       pums_h = pums_h, 
                                       pums_p = pums_p, 
                                       schools = schools, 
                                       workplaces = workplaces, 
                                       marginals = marginals, 
                                       output_type = output_type,
                                       sampling_method = sampling_method, 
                                       locations_method = locations_method, 
                                       convert_count = convert_count, 
                                       output_dir = output_dir,
                                       road_noise = road_noise, 
                                       timer = timer, 
                                       verbose = verbose)
  }
  
  return(region_list)
}


#' Run SPEW in Parallel with a SOCK backend 
#' 
#' Internal function, only called by the main spew function
#' 
#' @param num_places The number of regions (place_ids) to be generated for a given location 
#' @param pop_table dataframe where rows correspond to places where populations 
#' should be generated. Other requird columns are "n_house" and "puma_id"
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
#' @param output_type Default is "console" if we want to resulting population 
#' as an R variable. Alternative is "write", which is used on Olympus for writing 
#' out .csv files of the population
#' @param output_dir directory to write output if output_type = "write", NULL otherwise 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param sampling_method character indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param outfile_loc Defaults to "", so we print out the parallel run information. 
#' Only set to "/dev/null" for internal testing putposes.
#' @param export_objects objects for exporting to parallel backends 
#' @param road_noise Noise added to households during road-based sampling 
#' @param timer logical indicating we want to time the run 
#' @param verbose logical indicating we want to print output during the run.  Default is FALSE.#' 
#' 
#' @return region_list with output_type for all num_place locations 
spew_sock <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                      schools, workplaces, marginals, output_type, output_dir, 
                      convert_count, sampling_method, locations_method, outfile_loc, 
                      export_objects, road_noise, timer, verbose) {
  
  # Set up a SOCK cluster 
  num_workers <- min(num_places, parallel::detectCores(), 64) 
  cluster <- parallel::makeCluster(num_workers, type = "SOCK", outfile = outfile_loc)
  doParallel::registerDoParallel(cluster)
  
  # Export objects to the SOCK cluster  
  parallel::clusterExport(cl = cluster, varlist = export_objects, envir = environment())  
  
  # Run for-each to generate each place on a separate core 
  `%dopar%` <- foreach::`%dopar%`
  place <-  NULL
  region_list <- foreach::foreach(place = 1:num_places, 
                         .packages = c("plyr", "methods", "sp", "rgeos", "data.table", "mipfp", "quadprog"),
                         .errorhandling = 'pass', 
                         .export = export_objects) %dopar% {
                           
                         print(paste0("Region ", place, " out of ", num_places))
                         times <- spew_place(index = get("place"), 
                                    pop_table = pop_table, 
                                    shapefile = shapefile, 
                                    pums_h = pums_h, 
                                    pums_p = pums_p, 
                                    schools = schools, 
                                    workplaces = workplaces, 
                                    marginals = marginals, 
                                    output_type = output_type,
                                    sampling_method = sampling_method, 
                                    locations_method = locations_method, 
                                    convert_count = convert_count, 
                                    output_dir = output_dir,
                                    road_noise = road_noise, 
                                    timer = timer, 
                                    verbose = verbose)
                                                      
                           print(paste0("Finished ", pop_table[place, "place_id"]))
                           times
                         }
  
  parallel::stopCluster(cluster)
  return(region_list)
}

#' Run SPEW in Parallel with a Multicore backend 
#' 
#' Internal function, only called by the main spew function
#' 
#' @param num_places The number of regions (place_ids) to be generated for a given location 
#' @param pop_table dataframe where rows correspond to places where populations 
#' should be generated. Other requird columns are "n_house" and "puma_id"
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
#' @param output_type Default is "console" if we want to resulting population 
#' as an R variable. Alternative is "write", which is used on Olympus for writing 
#' out .csv files of the population
#' @param output_dir directory to write output if output_type = "write", NULL otherwise 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param sampling_method character indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param outfile_loc Defaults to "", so we print out the parallel run information. 
#' Only set to "/dev/null" for internal testing putposes.
#' @param export_objects objects for exporting to parallel backends 
#' @param road_noise Noise added to households during road-based sampling 
#' @param timer logical indicating we want to time the run 
#' @param verbose logical indicating we want to print output during the run.  Default is FALSE.#' 
#' 
#' @return region_list with output_type for all num_place locations 
spew_mc <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                    schools, workplaces, marginals, output_type, output_dir, 
                    convert_count, sampling_method, locations_method, outfile_loc, 
                    export_objects, road_noise, timer, verbose) {
  
  num_workers <- parallel::detectCores()
  region_list <- parallel::mclapply(X = 1:num_places, 
                                    FUN = spew_place, 
                                    pop_table = pop_table, 
                                    shapefile = shapefile, 
                                    pums_h = pums_h, 
                                    pums_p = pums_p, 
                                    schools = schools, 
                                    workplaces = workplaces, 
                                    marginals = marginals, 
                                    output_type = output_type,
                                    sampling_method = sampling_method, 
                                    locations_method = locations_method, 
                                    convert_count = convert_count, 
                                    output_dir = output_dir,
                                    road_noise = road_noise, 
                                    timer = timer, 
                                    verbose = verbose)
  
  return(region_list)
}

#' Run SPEW in Parallel with an MPI backend 
#' 
#' Internal function, only called by the main spew function
#' 
#' @param num_places The number of regions (place_ids) to be generated for a given location 
#' @param pop_table dataframe where rows correspond to places where populations 
#' should be generated. Other requird columns are "n_house" and "puma_id"
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
#' @param output_type Default is "console" if we want to resulting population 
#' as an R variable. Alternative is "write", which is used on Olympus for writing 
#' out .csv files of the population
#' @param output_dir directory to write output if output_type = "write", NULL otherwise 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param sampling_method character indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param outfile_loc Defaults to "", so we print out the parallel run information. 
#' Only set to "/dev/null" for internal testing putposes.
#' @param export_objects objects for exporting to parallel backends 
#' @param road_noise Noise added to households during road-based sampling 
#' @param timer logical indicating we want to time the run 
#' @param verbose logical indicating we want to print output during the run.  Default is FALSE.#' 
#' 
#' @return region_list with output_type for all num_place locations 
spew_mpi <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                     schools, workplaces, marginals, output_type, output_dir, 
                     convert_count, sampling_method, locations_method, outfile_loc, 
                     export_objects, road_noise, timer, verbose) {
  # Set up the MPI workers
  num_workers <- Rmpi::mpi.universe.size()
  Rmpi::mpi.spawn.Rslaves(nslaves = num_workers)
  
  # Print out the processor information
  rk <- Rmpi::mpi.comm.rank(0)
  sz <- Rmpi::mpi.comm.size(0)
  name <- Rmpi::mpi.get.processor.name()
  if (verbose) { cat("Hello, rank " , rk , " out of " , sz , " on " , name, "\n") }

  # If we are on Olympus, make sure to switch to personal libraries
  # because the core olympus directories don't have our updated functions. 
  hostname <- system("hostname", intern = TRUE)
  print(paste0("Hostname: ", hostname))
  if (length(grep("olympus", hostname)) == 1) {
    print("On Olympus, switching lib-paths")
    username <- system("whoami", intern = TRUE)
    print(paste0("Username: ", username))
    print(.libPaths())
    Rmpi::mpi.bcast.cmd(username <- system("whoami", intern = TRUE))    
    Rmpi::mpi.bcast.cmd(personal_lib <- grep(username, .libPaths()))
    Rmpi::mpi.bcast.cmd(print(.libPaths()[personal_lib]))
    Rmpi::mpi.bcast.cmd(print(personal_lib))
    Rmpi::mpi.bcast.cmd(.libPaths(new = c(.libPaths()[personal_lib])))
    Rmpi::mpi.bcast.cmd(print("New Lib-Paths"))
    Rmpi::mpi.bcast.cmd(print(.libPaths()))
  }
  
  # Send the relevant data objects/packes to workers 
  Rmpi::mpi.bcast.cmd("requireNamespace(plyr)")
  Rmpi::mpi.bcast.cmd("requireNamespace(methods)")
  Rmpi::mpi.bcast.cmd("requireNamespace(sp)")
  Rmpi::mpi.bcast.cmd("requireNamespace(rgeos)")
  Rmpi::mpi.bcast.cmd("requireNamespace(data.table)")
  Rmpi::mpi.bcast.cmd("requireNamespace(mipfp)")
  Rmpi::mpi.bcast.cmd("requireNamespace(quadprog)")
  
  # Export functions to all of the workers ------
  for (obj in export_objects) {
    if (verbose) { cat("Sending To Workers: ", obj, fill = TRUE) }
    do.call("mpi.bcast.Robj2slave" , list (obj = as.name(obj)))
  }
  
  # Call SPEW for each one of the places 
  region_list <- Rmpi::mpi.applyLB(X = 1:num_places, 
                             FUN = spew_place, 
                             pop_table = pop_table, 
                             shapefile = shapefile, 
                             pums_h = pums_h, 
                             pums_p = pums_p, 
                             schools = schools, 
                             workplaces = workplaces, 
                             marginals = marginals, 
                             output_type = output_type,
                             sampling_method = sampling_method, 
                             locations_method = locations_method, 
                             convert_count = convert_count, 
                             output_dir = output_dir,
                             road_noise = road_noise, 
                             timer = timer, 
                             verbose = verbose)
  
  # Close the connections and MPI
  Rmpi::mpi.close.Rslaves()
  Rmpi::mpi.exit()
  
  return(region_list)
}

#' Generate synthetic ecosystem for single place 
#' 
#' @param index specfic row of pop-table to generate 
#' @param pop_table dataframe where rows correspond to places where populations 
#' should be generated. Other requird columns are "n_house" and "puma_id"
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
#' @param output_type Default is "console" if we want to resulting population 
#' as an R variable. Alternative is "write", which is used on Olympus for writing 
#' out .csv files of the population
#' @param sampling_method character indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data. 
#' @param locations_method character indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households 
#' @param output_dir directory to write output if output_type = "write", NULL otherwise 
#' @param road_noise Noise added to households during road-based sampling 
#' @param timer logical indicating we want to time the run 
#' @param verbose logical indicating we want to print output during the run.  Default is FALSE.#' 
#' 
#' @return List of a synthetic ecosystem 
spew_place <- function(index, pop_table, shapefile, pums_h, pums_p, 
                       schools = NULL, workplaces = NULL, marginals = NULL, 
                       output_type = "console", sampling_method = "uniform", 
                       locations_method = "uniform", convert_count = FALSE, 
                       output_dir = NULL, road_noise = .0002, 
                       timer = TRUE, verbose = FALSE) {
  if (timer == TRUE) { place_start_time <- Sys.time() }

  # Obtain number of households, puma_id, and place_id from pop table
  n_house <- pop_table[index, "n_house"]
  puma_id <- pop_table[index, "puma_id"]
  place_id <- pop_table[index, "place_id"]
  
  # To avoid data-frames larger than 5 Million, generate large 
  # places incrementally, appending rows onto each-other 
  append <- FALSE
  max_size <- 5000000
  iter <- 0
  total_hh <- 0
  total_people <- 0
  number_iters <- floor(n_house / max_size)
  if (number_iters == 0) {
    iter_vec <- n_house
  } else {
    remainder <- n_house %% max_size 
    iter_vec <- c(rep(max_size, number_iters), remainder)
  }
  
  for (n_house in iter_vec) {
    # If this is the second increment of this place, make sure we append
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
    
    # Sample Households ---
    sampled_households <- sample_households(method = sampling_method, 
                                            n_house = n_house, 
                                            pums_h = pums_h, 
                                            pums_p = pums_p, 
                                            marginals = marginals,
                                            puma_id = puma_id, 
                                            place_id = place_id)
    total_hh <- total_hh + nrow(sampled_households)

    # Sample Household Locations ---
    locations <- sample_locations(method = locations_method, 
                                  place_id = place_id,
                                  n_house = n_house, 
                                  shapefile = shapefile, 
                                  noise = road_noise, 
                                  shapefile_id = shapefile_id)
    sampled_households$longitude <- locations@coords[, 1]
    sampled_households$latitude <- locations@coords[, 2]
    rm(locations); gc()
    
    # Sample People --- 
    sampled_people <- sample_people(method = sampling_method, 
                                    household_pums = sampled_households, 
                                    pums_p = pums_p, 
                                    puma_id = puma_id, 
                                    place_id = place_id)
    total_people <- total_people + nrow(sampled_people)
    
    # Schools --- 
    school_time <- 0
    if (!is.null(schools)) {
      school_start_time <- Sys.time()
      
      school_ids <- assign_schools(sampled_people, schools)
      sampled_people$school_id <- school_ids
      stopifnot("school_id" %in% names(sampled_people))
      
      school_time <- difftime(Sys.time(), school_start_time, units = "secs")
      school_time <- round(school_time, digits = 2)
    }
    
    # Workplaces ---
    workplace_time <- 0
    if (!is.null(workplaces)) {
      workplace_start_time <- Sys.time()
      
      workplace_ids <- assign_workplaces(sampled_people, workplaces)
      sampled_people$workplace_id <- workplace_ids
      stopifnot("workplace_id" %in% names(sampled_people))
      
      workplace_time <- difftime(Sys.time(), workplace_start_time, units = "secs")
      workplace_time <- round(school_time, digits = 2)
    }
    
    # Activities ---
    # ABBYS FUNCTION GOES HERE 
    
    # Return synthetic ecosystem if output_type equals 'console'
    if (output_type == "console") {
      return(list(households = sampled_households,
                  people = sampled_people, 
                  place_id = place_id,
                  puma_id = puma_id))
    }
    
    stopifnot(output_type == "write")
    
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
  
  hh_statement <- paste0("Households: ", total_hh)
  people_statement <- paste0("People: ", total_people)  
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
#' @param append logical determining if we want to append to the current file 
#' 
#' @return data indicating the indices of people to sample 
write_data <- function(df, place_id, puma_id, type, output_dir, append = FALSE) {
  # Create the output directory for this PUMA 
  directory <- file.path(output_dir, paste0("output_", puma_id), "eco")  
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)          
  }
  
  filename <- file.path(directory, paste0(type, "_", as.character(place_id), ".csv"))
  filename <- remove_excess(filename)
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    write.csv(df, filename, append = append)
  } else {
    data.table::fwrite(df, filename, sep = ",", qmethod = "double", append = append)
  }
  
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
#' @param region_list a list containing all of the 
#' summary and diagnostic information for each  
#'  
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
