#' Run SPEW in Parallel with an MPI backend 
spew_mpi <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                     schools, workplaces, marginals, sampling_method, 
                     locations_method, convert_count, output_dir, 
                     export_objects, do_subset_pums = TRUE,
                     do_write = TRUE, noise = noise) {
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
  if (length(grep("olympus", hostname)) == 1) {
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
  mpi.bcast.cmd(requireNamespace(plyr))
  mpi.bcast.cmd(requireNamespace(methods))
  mpi.bcast.cmd(requireNamespace(sp))
  mpi.bcast.cmd(requireNamespace(rgeos))
  mpi.bcast.cmd(requireNamespace(data.table))
  mpi.bcast.cmd(requireNamespace(mipfp))
  mpi.bcast.cmd(requireNamespace(quadprog))
  
  # Export functions to all of the workers ------
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
                             output_dir = output_dir, 
                             do_subset_pums = do_subset_pums,
                             do_write = do_write, noise = noise)
  
  # Close the connections and MPI
  mpi.close.Rslaves()
  mpi.exit()
  
  return(region_list)
}

#' Run SPEW in Parallel with a SOCK backend 
spew_sock <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                      schools, workplaces, marginals, sampling_method, 
                      locations_method, convert_count, output_dir, outfile_loc, 
                      export_objects, do_subset_pums = TRUE,
                      do_write = TRUE, noise = .0002) {
  # Set up a SOCK cluster 
  num_workers <- min(num_places, parallel::detectCores(), 64) 
  cluster <- makeCluster(num_workers, type = "SOCK", outfile = outfile_loc)
  doParallel::registerDoParallel(cluster)
  
  # Export objects to the SOCK cluster  
  parallel::clusterExport(cl = cluster, varlist = export_objects, envir = environment())  
  
  # Run for-each to generate each place on a separate core 
  region_list <- foreach(place = 1:num_places, 
                         .packages = c("plyr", "methods", "sp", "rgeos", 
                                       "data.table", "mipfp", "quadprog"),
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
                                               output_dir = output_dir,
                                               do_subset_pums = do_subset_pums,
                                               do_write = do_write, noise = noise)
                           print(paste0("Finished ", pop_table[place, "place_id"]))
                           times
                         }
  stopCluster(cluster)
  
  return(region_list)
}

#' Run SPEW in Parallel with a Multicore backend 
spew_mc <- function(num_places, pop_table, shapefile, pums_h, pums_p, 
                    schools, workplaces, marginals, sampling_method, 
                    locations_method, convert_count, output_dir, outfile_loc,
                    do_subset_pums = TRUE, do_write = TRUE, noise = .0002) {
  
  num_workers <- detectCores()
  region_list <- mclapply(X = 1:num_places, 
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
                          output_dir = output_dir, 
                          mc.cores = num_workers,
                          do_subset_pums = do_subset_pums,
                          do_write = do_write, noise = noise)
  
  return(region_list)
}
