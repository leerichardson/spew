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
#' @param sampling_type character vector indicating the type oof sampling used. 
#' Default's to "uniform"
#' @param output_dir character vector specifying where to write the synthetic microdata 
#' @return logical specifying whether the microdata was generated 
#' successfully 
#' @examples
#' make_data(sd_data$pop_table, sd_data$shapefiles, sd_data$pums$pums_h, sd_data$pums$pums_p)
make_data <- function(pop_table, shapefile, pums_h, pums_p, schools, workplaces, 
                      parallel = FALSE, sampling_type = "uniform", 
                      output_dir , convert_count) {
  
  start_time <- Sys.time()
  
  # Write out the final, formatted population table  
  write_pop_table(pop_table, output_dir)
  
  # Call the make_place function for each place in our pop_table. Either 
  # run this in parallel of not (usually I don't for debugging purposes)
  num_places <- nrow(pop_table) 
  
  if (parallel == FALSE) {
    
    for (place in 1:num_places) { 
      make_place(place, pop_table, shapefile, pums_h, pums_p, schools, 
                 workplaces, sampling_type, output_dir, convert_count) 
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
                        "assign_workplaces", "assign_workplaces_inner", "remove_holes")
    
    parallel::clusterExport(cl = cluster, varlist = export_objects, envir = environment())    
    doSNOW::registerDoSNOW(cluster)
    
    foreach(place = 1:num_places, .packages = c("plyr"), .export = export_objects) %dopar% {
      
      # Print out relevant information pertaining to the job
      make_place(place, pop_table, shapefile, pums_h, pums_p, schools, 
                 workplaces, sampling_type, output_dir, convert_count) 
    }
    
    parallel::stopCluster(cluster)
  }
  
  # Print the diagnostics and summaries of the entire place 
  overall_time <- difftime(Sys.time(), start_time, units = "secs")
  overall_time <- round(overall_time, digits = 2)  
  time_statement <- paste0("Time Running Make Data: ", overall_time)
  
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
#' @return synthetic population .csv file for both household and person 
#' level data  
make_place <- function(index, pop_table, shapefile, pums_h, pums_p, schools,
                       workplaces, sampling_type, output_dir, convert_count) {

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
  if (class(shapefile) == "list") {
      stopifnot(any(names(shapefile) == "roads"))
      locations <- sample_locations_from_roads(place_id = place_id,
                                               n_house = n_house, 
                                               shapefile = shapefile, 
                                               noise = .0002)
  } else {
      locations <- sample_locations(place_id = place_id, 
                                    n_house = n_house, 
                                    shapefile = shapefile)
  }
  
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
  school_msg <- "no"
  if (!is.null(schools)) {
    school_msg <- "yes"
    school_ids <- assign_schools(sampled_people, schools)
    sampled_people$school_id <- school_ids
    stopifnot("school_id" %in% names(sampled_people))
  }

  # Assign workplaces to people if the data exists 
  workplace_msg <- "no"
  if (!is.null(workplaces)) {
    workplace_msg <- "yes"
    workplace_ids <- assign_workplaces(sampled_people, workplaces)
    sampled_people$workplace_id <- workplace_ids
    stopifnot("workplace_id" %in% names(sampled_people))
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
  
  msg <- paste0("Place: ", index, " out of ", nrow(pop_table))        
  
  total_households <- nrow(sampled_households)
  total_people <- nrow(sampled_people)
  
  hh_statement <- paste0("Households: ", total_households)
  people_statement <- paste0("People: ", total_people)
  time_statement <- paste0("Time: ", overall_time)
  school_statement <- paste0("Schools: ", school_msg)
  workplace_statement <- paste0("Workplaces: ", workplace_msg)  
  
  print(msg)
  print(paste0("Place Name: ", place_id))
  print(paste0("Puma: ", puma_id))
  print(hh_statement)
  print(people_statement)
  print(time_statement)
  print(school_statement)
  print(workplace_statement)
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
#' @param n_house numeric indicating the number of households
#' @param shapefile sp class with all of the locations for each place id
#' @return SpatialPoints object with coordinates for the n households
sample_locations <- function(place_id, n_house, shapefile) {
  # Subset the shapefile to the polygon 
  # specified by the place_id argument 
  slots <- methods::slot(shapefile, "polygons")
  region <- which(shapefile$place_id == place_id)
  poly <- slots[[region]]
  
  # Remove holes from polygon if any are found 
  is_hole <- lapply(poly@Polygons, function(p) p@hole)
  if (any(unlist(is_hole)) == TRUE) {
    poly <- remove_holes(poly)
  }
  
  # Obtain a Uniform, simple random sample of size n_house
  locs <- sp::spsample(poly, n = n_house, offset = c(0, 0), 
                       type = "random", iter = 50)
  return(locs)
}

#' Remove holes from an object of class Polygon 
#' 
#' @param polygon object of class Polygon or Polygons 
#' 
#' @note Borrowed the idea from the wild1 package, which 
#' I wasn't able to load for R 3.2.2, so I found the source code here:
#' https://github.com/cran/wild1/blob/master/R/remove.holes.r
#' @return polygon without and Polygons with holes 
remove_holes <- function(polygon) {
  is_hole <- lapply(polygon@Polygons, function(p) p@hole)
  is_hole <- unlist(is_hole)
  polys <- polygon@Polygons[!is_hole]
  polygon <- Polygons(polys, ID = polygon@ID)
  return(polygon)
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
    
#' Sample coordinates from roads
#'
#' @param place_id numeric specifiying the ID of the region we are 
#' subsampling  
#' @param n_house numeric indicating the number of households
#' @param shapefile sp class with all of the locations for each place id.  
#' In addition, we must have road shapefiles so shapefile is a list with both 
#' the tracts and the roads, tracts is the first object and roads the second.
#' @param noise the standard deviation of how much we jitter the road locations in each direction
#' @return SpatialPoints object with coordinates for the n households
sample_locations_from_roads <- function(place_id, n_house, shapefile, noise = .0001) {
    newShp <- subset_shapes_roads(place_id, shapefile)
    locs <- samp_roads(n_house, newShp, noise)
    return(locs)
}

#' Subset the shapefile and road lines to proper roads within specified tract
#'
#' @param place_id numeric specifiying the ID of the region we are 
#' subsampling  
#' @param shapefile sp class with all of the locations for each place id.  
#' In addition, we must have road shapefiles so shapefile is a list with both the 
#' tracts and the roads, tracts is the first object and roads the second.
#' @return newShp - roads within the tract, a SpatialLines object
subset_shapes_roads <- function(place_id, shapefile){
    stopifnot(class(shapefile) == "list")
    stopifnot(length(shapefile) == 2)

    # Subset the regions to the place_id polygon
    regions <- shapefile[[1]]
    poly <- regions[regions@data$place_id == place_id, ]

    # Add the roads in to make a SpatialLines Object
    roads_sub <- shapefile[[2]][poly,]
    newShp <- rgeos::gIntersection(roads_sub, poly)
    return(newShp)
}

#' Sample the locations from the liens of a SpatialLines object
#'
#' @param n_house number of households
#' @param newShp SpatialLines object
#' @param noise std deviation of Gaussian noise added to coordinates, default is .001
#' @return SpatialPoints object with coordinates for the n_house households
samp_roads <- function(n_house, newShp, noise){
    if( "lineobj" %in% slotNames(lines)){
        pts <- sp::spsample(newShp@lineobj, n = n_house, type = "random", iter = 50)
    } else {
        pts <- sp::spsample(newShp, n = n_house, type = "random", iter = 50)
    }

    # Sometimes sampling fails.  If so, we resample from already 
    # selected points to fill in the rest.

    if (n_house != length(pts)){
        resampled_pts <- n_house - length(pts)
        inds <- sample(1:length(pts), resampled_pts)
        pts <- pts[c(1:length(pts), inds),]
    }

    err_x <- rnorm(length(pts), 0, noise)
    err_y <- rnorm(length(pts), 0, noise)
    pts@coords[,1] <- pts@coords[,1] + err_x
    pts@coords[,2] <- pts@coords[,2] + err_y

    stopifnot(n_house == length(pts))
    return(pts)
}
