#' Read in the SPEW input data
#' 
#' Based on the hierarchy on the Olympus Supercomputer
#' 
#' @param base_dir character vector specifying the ecosystem directory 
#' @param folders list containing the path of each data sub directory 
#' @param data_group character either "US", "ipums" or "none" which tells 
#' read_data if the input data follows a particular format. Used mainly for 
#' the pre-formatted data-types we have on our Olympus
#' @param vars list with two components: household and person. This specifies 
#' which variables to include in the corresponding PUMS data-set  
#' @return list in which each element contains one of our standardized 
#' data sources
read_data <- function(base_dir, 
                      folders = list(pop_table = NULL, 
                                     pums = NULL, 
                                     shapefiles = NULL, 
                                     roads = NULL, 
                                     schools = NULL, 
                                     lookup = NULL, 
                                     workplaces = NULL), 
                      data_group = "US", 
                      vars = list(household = NA, person = NA)) {
  read_start_time <- Sys.time()
  
  if (data_group != "US" & data_group != "ipums" & data_group != "none") {
    stop("spew only accepts data_group: 'US', 'ipums', or 'none'")
  } 
  
  # Point the input directory to the input/ portion of the base directory
  input_dir <- file.path(base_dir, "input")
  
  # Read in each source one by one -------------------------
  pop_table <- read_pop_table(input_dir, folders, data_group)
  pop_table <- standardize_pop_table(pop_table, data_group)
  
  pums <- read_pums(input_dir, folders, data_group, vars)
  pums <- standardize_pums(pums, data_group)
  
  shapefiles <- read_shapefiles(input_dir, folders, data_group)
  shapefiles <- standardize_shapefiles(shapefiles, data_group)
  
  if (!is.null(folders$lookup)) {
    lookup <- read_lookup(input_dir, folders, data_group)
    lookup <- standardize_lookup(lookup, data_group)
  } else {
    lookup <- NULL
  }
  
  if (!is.null(folders$schools)) {
    schools <- read_schools(input_dir, folders, data_group)
  } else {
    schools <- NULL
  }
  
  if (!is.null(folders$workplaces)) {
    workplaces <- read_workplaces(input_dir, folders, data_group)
  } else {
    workplaces <- NULL
  }


  if (!is.null(folders$marginals)) {
    marginals <- read_marginals(input_dir, folders, data_group)
  } else {
    marginals <- NULL
  }

  if(!is.null(folders$moments)){
      moments <- read_moments(input_dir, folders, data_group)
  } else {
      moments <- NULL
  }
  
  read_time <- difftime(Sys.time(), read_start_time, units = "secs")
  read_time <- round(read_time, digits = 2)  
  read_time_statement <- paste0("Read runs in: ", read_time)
  print(read_time_statement)
  
  return(list(pop_table = pop_table, 
              pums = pums, 
              lookup = lookup, 
              shapefiles = shapefiles, 
              schools = schools, 
              workplaces = workplaces, 
              marginals = marginals,
              moments = moments))
}

#' Read in the population counts  
#' 
#' @param input_dir character vector specifying the directory containing 
#' all of the input data 
#' @param folders list which contains the path of each sub-directory with the 
#' specific datadelaware_marginals
#' @param data_group character either "US", "ipums" or "none" which tells 
#' read_data if the input data follows a particular format. Used mainly for 
#' the pre-formatted data-types we have on our Olympus
#' 
#' @return data frame with counts 
read_pop_table <- function(input_dir, folders, data_group) {
  pop_table_dir <- file.path(input_dir, folders$pop_table)
  pop_table_files <- list.files(pop_table_dir)
  
  if (data_group == "US") {
    # For US, should always be households.csv
    pop_table_file <- file.path(pop_table_dir, "households.csv")
  } else if (data_group == "ipums") {
    # If the extended counts are available, use them 
    # If not, use the available admin counts 
    pop_table_files <- list.files(pop_table_dir, 
                                  all.files = TRUE, 
                                  recursive = TRUE, 
                                  full.names = TRUE)
    
    revised_counts <- grep("revised", pop_table_files)
    extended_counts <- grep("extended", pop_table_files)
    admin_counts <- grep("admin", pop_table_files)
    
    if (length(revised_counts) != 0) {
      pop_table_file <- pop_table_files[revised_counts]
    } else if (length(extended_counts != 0)) {
      pop_table_file <- pop_table_files[extended_counts]
    } else if (length(admin_counts) != 0) {
      pop_table_file <- pop_table_files[admin_counts]
    } else {
      stop("There is no revised, admin or extended counts!")
    }
    
  } else if (data_group == "none") {
    pop_table <- read.csv(folders$pop_table, stringsAsFactors = FALSE, colClasses = "character")
    pop_table$n_house <- as.numeric(pop_table$n_house)
    return(pop_table)
  }
  
  pop_table <- read.csv(pop_table_file, stringsAsFactors = FALSE)
  return(pop_table)
}

#' Make sure pop_table has the appropriate columns
standardize_pop_table <- function(pop_table, data_group){
    if (data_group == "US") {
    # Make sure we aren't stripping off a leading 0 
    # if we are dealing with one of the first 10 states 
    if (all(nchar(pop_table$Id2) == 10)) {
      pop_table$Id2 <- paste0("0", pop_table$Id2)
    }     
    stopifnot(all(nchar(pop_table$Id2) == 11))
    
    pop_table <- data.frame(place_id = pop_table$Id2,
                            n_house = as.numeric(pop_table$NumberOfHouseholds))
    
    # Make sure the variables have the correct class
    pop_table$place_id <- as.character(pop_table$place_id)
    pop_table$n_house <- as.numeric(pop_table$n_house)
  } else if (data_group == "ipums") {
      if (all(names(pop_table) == c("place_id", "n_house", "level"))) {
        return(pop_table)
      } else {
        # Extract the rows with the most recent counts, 
        # names of countries, and total numbers
        final_row <- which(pop_table$level == "total")
        cols <- ncol(pop_table)
        
        # Make sure we are getting the name instead of the code, 
        # then re-name the columns to their formatted names 
        name_col <- 1
        if (names(pop_table)[1] == "code") {
          name_col <- name_col + 1
        }
        
        pop_table <- pop_table[1:final_row, c(name_col, cols - 1, cols)]
        names(pop_table) <- c("place_id", "n_house", "level")
      }
  } else if (data_group == "none") {
    check_pop_table(pop_table)
  }
  
  return(pop_table)
}

# Read PUMS data --------------------
read_pums <- function(input_dir, folders, data_group, vars = list(household = NA, person = NA)) {
  pums_dir <- file.path(input_dir, folders$pums)
  pums_files <- list.files(pums_dir)
  
  if (data_group == "US") {
    #  Find the indices of the person and household level files
    hp <- substr(pums_files, 5, 5)
    index_h <- which(hp == "h")
    index_p <- which(hp == "p")
    
    #  Read in the person and household level files
    household_file <- paste0(input_dir, "/", folders$pums, "/", pums_files[index_h])
    pums_h <- read.csv(household_file, stringsAsFactors = FALSE)
    people_file <- paste0(input_dir, "/", folders$pums, "/", pums_files[index_p])
    pums_p <- read.csv(people_file, stringsAsFactors = FALSE)
    
  } else if (data_group == "ipums") {
    pums_files <- list.files(pums_dir, 
                                  all.files = TRUE, 
                                  recursive = TRUE, 
                                  full.names = TRUE)
    stopifnot(length(pums_files) == 1)
    
    # Use the unique household ID's for household pums  
    pums_p <- fread(pums_files, stringsAsFactors = FALSE)
    
    # Verify none of the classes are integer64
#     int64_inds <- which(unlist(lapply(pums_p, class)) == "integer64")
#     pums_p[, int64_inds] <- as.character(pums_p[, int64_inds])    
    
    unique_hh_indices <- !duplicated(pums_p$SERIAL)
    pums_h <- pums_p[unique_hh_indices, ]
  
  } else if (data_group == "none") {
    pums_h <- read.csv(folders$pums$pums_h, stringsAsFactors = FALSE)
    pums_p <- read.csv(folders$pums$pums_p, stringsAsFactors = FALSE)
    
  }

  # Subset the Household and Person Variables 
  if (!is.na(vars$household)[1]) {
    pums_h <- pums_h[, vars$household]  
  }

  if (!is.na(vars$person)[1]) {
    pums_p <- pums_p[, vars$person]  
  } 
  
  return(list(pums_h = pums_h, pums_p = pums_p))
}

#  Standardize the pums data 
standardize_pums <- function(pums, data_group) {
  if (data_group == "US") {
    names(pums$pums_h)[which(names(pums$pums_h) == "PUMA")] <- "puma_id"
    names(pums$pums_p)[which(names(pums$pums_p) == "PUMA")] <- "puma_id"
  
  } else if (data_group == "ipums") {
    # Set the puma_id and serial ID's 
    names(pums$pums_h)[which(names(pums$pums_h) == "GEOLEV1")] <- "puma_id"  
    names(pums$pums_h)[which(names(pums$pums_h) == "SERIAL")] <- "SERIALNO"
    names(pums$pums_p)[which(names(pums$pums_p) == "SERIAL")] <- "SERIALNO"
  
  } else if (data_group == "none") {
    check_pums(pums)
  }
  
  return(pums)
}

#  Function for reading in lookup data
read_lookup <- function(input_dir, folders, data_group){
  lookup_files <- list.files(paste0(input_dir, "/", folders$lookup))
  
  if (data_group == "US") {
    filename <- "lookup10.csv"
  } else if (data_group == "ipums") {
    
  } else if (data_group == "none") {
    
  }  
  
  lookup_file <- paste0(input_dir, "/", folders$lookup, "/", filename)
  lookup <- read.csv(lookup_file, stringsAsFactors = FALSE)
  return(lookup)
}

#  Standardize the lookup table
standardize_lookup <- function(lookup, data_group){
  if (data_group == "US") {
    #  Below, we add 100, 1000, and 1000000 and then take the substrings
    #  so that each string has the same length
    new_state_fp <- lookup$STATEFP + 100  
    new_state_fp <- substr(new_state_fp, 2, 3)
    new_county_fp <- lookup$COUNTYFP + 1000
    new_county_fp <- substr(new_county_fp, 2, 4)
    new_tract_ce <- lookup$TRACTCE + 1000000
    new_tract_ce <- substr(new_tract_ce, 2, 7)
    place_id <- paste0(new_state_fp, new_county_fp, new_tract_ce)
    lookup <- data.frame(place_id = as.character(place_id),
                         puma_id = as.numeric(lookup$PUMA5CE))
    
    # Make sure the variables have the correct class
    lookup$place_id <- as.character(lookup$place_id)
    lookup$puma_id <- as.numeric(lookup$puma_id)
  }
  return(lookup)
}

#  Function for reading in shapefiles data
read_shapefiles <- function(input_dir, folders, data_group) {
  shapefile_dir <- file.path(input_dir, folders$shapefiles)
  shapefiles_files <- list.files(shapefile_dir)
   
  if (data_group == "US") {
    ind_shp <- which(grepl(pattern = "\\.shp", x = shapefiles_files) & 
                       !grepl(pattern = "\\.xml", x = shapefiles_files)) 
    filename <- shapefiles_files[ind_shp]
    full_path <- file.path(input_dir, folders$shapefiles, "/", filename)
    shapefile <- maptools::readShapeSpatial(full_path)
      
    # If road_data, read in its file-path. Otherwise, return shapefile 
    if (!is.null(folders$roads)) {
        roads_path <- file.path(input_dir, folders$roads)           
        return(list(shapefile = shapefile, roads = roads_path))
    } else {
      return(shapefile)
    }

  } else if (data_group == "ipums") {
    shapefiles <- list.files(shapefile_dir, 
                              all.files = TRUE, 
                              recursive = TRUE, 
                              full.names = TRUE)
    revised_indices <- grep("revised.shp", shapefiles)
    
    if (length(revised_indices) == 1) {
      filename <- shapefiles[revised_indices]
    } else {
      other_shapes <- grep(".shp", shapefiles)
      ipums_shapes <- grep("ipums", shapefiles)
      shp_indices <- intersect(other_shapes, ipums_shapes)
      stopifnot(length(shp_indices) == 1)
      filename <- shapefiles[shp_indices]
    }
    shapefile <- maptools::readShapeSpatial(filename)
    return(shapefile)
    
  } else if (data_group == "none") {
    shapefile <- maptools::readShapeSpatial(folders$shapefiles)
    return(shapefile)
  }
}

#  Standardize the shapefiles 
standardize_shapefiles <- function(shapefiles, data_group) {
  if (data_group == "US") {
    if (class(shapefiles) == "SpatialPolygonsDataFrame") {
      names(shapefiles)[which(names(shapefiles) == "GEOID10")] <- "place_id"
      shapefiles$place_id  <- as.character(shapefiles$place_id)
      stopifnot(all(nchar(shapefiles$place_id) == 11))
    } else if (class(shapefiles) == "list") {
      reg_shapefiles <- which(names(shapefiles) == "shapefile")
      names(shapefiles[[reg_shapefiles]])[which(names(shapefiles[[reg_shapefiles]]) == "GEOID10")] <- "place_id"
      shapefiles[[reg_shapefiles]]$place_id  <- as.character(shapefiles[[reg_shapefiles]]$place_id)
      stopifnot(all(nchar(shapefiles[[reg_shapefiles]]$place_id) == 11))
    }
  
  } else if (data_group == "ipums") {
    names(shapefiles)[which(names(shapefiles) == "ADMIN_NAME")] <- "place_id"
    shapefiles$place_id  <- as.character(shapefiles$place_id)
    names(shapefiles)[which(names(shapefiles) == "GEOLEVEL1")] <- "puma_id"

  } else if (data_group == "none") {
    shapefiles$place_id <- as.character(shapefiles$place_id)
    check_shapefile(shapefiles)
  }
  
  return(shapefiles)
}

#  Function for reading in schools data
read_schools <- function(input_dir, folders, data_group) {
  if (data_group == "US") {
    schools_path <- paste0(input_dir, "/", folders$schools, "/")
    school_files <- list.files(schools_path)
    
    # Read in public and private school data-frames 
    public_file_index <- grep("public", school_files)
    public_file <- paste0(schools_path, school_files[public_file_index])
    public_df <- data.table::fread(public_file, stringsAsFactors = FALSE, data.table = FALSE, 
                          colClasses = c(StNo = "character", CoNo = "character", ID = "character"))
      
    private_file_index <- grep("private", school_files)
    private_file <- paste0(schools_path, school_files[private_file_index])
    private_df <- data.table::fread(private_file, stringsAsFactors = FALSE, data.table = FALSE,
                           colClasses = c(StNo = "character", CoNo = "character", ID = "character"))

    # Combine the public and private schools into a list 
    schools <- list(public = public_df, private = private_df)
    return(schools)
    
  } else if (data_group == "ipums") {
    
  } else if (data_group == "none") {
  
  }  
  
}

#  Function for reading in workplaces data
read_workplaces <- function(input_dir, folders, data_group) {
  workplace_files <- list.files(paste0(input_dir, "/", folders$workplaces))
  
  if (data_group == "US") {
    if (length(workplace_files == 1)) {
      filename <- workplace_files
      workplaces <- read.csv(paste0(input_dir, "/", folders$workplaces, "/", filename), stringsAsFactors = FALSE)

      # Make sure that the stcotr variable has 
      # 11 characters. If not, add a 0 in the beginning 
      geog <- as.character(workplaces$stcotr)
      if (all(nchar(geog) == 10)) {
        geog <- paste0("0", geog)
      }
      workplaces$stcotr <- geog
      
    } else if (length(workplace_files) > 1) {
      stop("Expecting only one file in the workplaces directory")
    }
    
  } else if (data_group == "ipums") {
    
  } else if (data_group == "none") {
  
  }
  
  return(workplaces)
}

#' Read in road lines shapefiles
#'
#' @param path_to_roads full path to the directory of where 
#' the roads are stored, the directory should have zip files that 
#' have been unzipped for each county
#' @return an appended SpatialDataFrame object with all the roads in the state
read_roads <- function(path_to_roads, road_id) {
    # Get a vector of the roads .shp files 
    road_files <- list.files(path_to_roads)
    road_shapes <- road_files[grepl("shp", road_files) & !grepl("xml", road_files)]
    shape_names <- substr(road_shapes, 9, 13)
    
    # Only subset the road_id 
    road <- which(shape_names == road_id)
    
    # If road ID is null, then return a NULL, which
    # will propogate sampling uniformly  
    if (length(road) == 0) {
      warning("No roads for this county")
      return(NULL)
    }
    
    path_to_road <- file.path(path_to_roads, road_shapes[road])
    road_shp <- maptools::readShapeSpatial(path_to_road)

    # Take out the interstates
    # US Specific
    interstate_inds <- which(as.character(road_shp@data$RTTYP) == "I")
    if (length(interstate_inds) > 0){
        road_shp <- road_shp[-interstate_inds,]
    }
    return(road_shp)
}

#' Read in the marginals population characteristic totals 
#' 
#' @param input_dir character vector specifying the directory containing 
#' all of the input data 
#' @param folders list which contains the path of each sub-directory with the 
#' specific data
#' @param data_group character either "US", "ipums" or "none" which tells 
#' read_data if the input data follows a particular format. Used mainly for 
#' the pre-formatted data-types we have on our Olympus
#' 
#' @return data frame with counts 
read_marginals <- function(input_dir, folders, data_group) {
                                        # Get a character vector of files in marginal folder
  marginal_files <- list.files(file.path(... = input_dir, folders$marginals))
  
  if (data_group == "US") {
    # Subset marginal file with "marginals" in name
    marginal_ind <- grep(pattern = "marginals", x = marginal_files)
    marginal_file <- marginal_files[marginal_ind]
    marginals <- readRDS(file = file.path(input_dir, folders$marginals, marginal_file))
    return(marginals)

  } else if (data_group == "ipums") {
    
  } else if (data_group == "none") {
      if(sampling_method == "ipf"){
            # Subset marginal file with "marginals" in name
          marginal_ind <- grep(pattern = "marginals", x = marginal_files)
          marginal_file <- marginal_files[marginal_ind]
          marginals <- readRDS(file = file.path(input_dir, folders$marginals, marginal_file))
          print("got marginals for none group ipf")
      }
    return(marginals)


  }
}

#' Read in the R data object for moment matching
#'
#' @param input_dir character vector specifying the directory containing 
#' all of the input data 
#' @param folders list which contains the path of each sub-directory with the 
#' specific data
#' @param data_group character either "US", "ipums" or "none" which tells 
#' read_data if the input data follows a particular format. Used mainly for 
#' the pre-formatted data-types we have on our Olympus
#' @return moment object
read_moments <- function(input_dir, folders, data_group){
    moments_files <- list.files(file.path(... = input_dir, folders$moments))
     
    ## Subset marginal file with "moments" in name
    marginal_ind <- grep(pattern = "mm", x = moments_files)
    marginal_file <- marginal_files[marginal_ind]
    moments <- readRDS(file = file.path(input_dir, folders$moments, marginal_file))
    return(moments)
}
