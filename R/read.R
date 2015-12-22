#  Sam Ventura, Lee Richardson, and Shannon Gallagher
#  3 November 2015
#  SPEW -- Synthetic Populations and Ecosystems of the World
#  Update:  11/10/15
#  Update:  11.12.15 - read in schools and workplaces

#' Read in all necessary data 
#' 
#' @param input_dir character vector specifying the directory containing 
#' all of the input data 
#' @param folders list which contains the path of each sub-directory with the 
#' specific data
#' @param data_group character either "US", "ipums" or "none" which tells 
#' read_data if the input data follows a particular format. Used mainly for 
#' the pre-formatted data-types we have on our Olympus
#' 
#' @return list in which each element contains one of our standardized 
#' data sources
read_data <- function(input_dir, 
                      folders = list(pop_table = "popTables", 
                                           pums = "pums", 
                                           schools = "schools", 
                                           lookup = "tables", 
                                           shapefiles = "tiger", 
                                           workplaces = "workplaces"),
                      data_group = "US") {
  
  if (data_group != "US" & data_group != "ipums"& data_group != "none") {
    stop("spew only accepts data_group: US, ipums, or none")
  } 
  
  #  Read in pop_table data
  pop_table <- read_pop_table(input_dir, folders, data_group)
  pop_table <- standardize_pop_table(pop_table, data_group)
  
  #  Read in pums data
  pums <- read_pums(input_dir, folders, data_group)
  pums <- standardize_pums(pums, data_group)
  
  #  Read in lookup data
  lookup <- read_lookup(input_dir, folders, data_group)
  lookup <- standardize_lookup(lookup, data_group)
  
  #  Read in shapefiles data
  shapefiles <- read_shapefiles(input_dir, folders, data_group)
  shapefiles <- standardize_shapefiles(shapefiles, data_group)
  
  #  Read in schools data, if necessary
  if (folders$schools %in% list.files(input_dir)) {
    schools <- read_schools(input_dir, folders, data_group)
  } else {
    schools <- NULL
  }
  
  #  Read in workplaces data, if necessary
  if (folders$workplaces %in% list.files(input_dir)) {
    workplaces <- read_workplaces(input_dir, folders, data_group)
  } else {
    workplaces <- NULL
  }
      
  return(list(pop_table = pop_table, 
            pums = pums, 
            lookup = lookup, 
            shapefiles = shapefiles, 
            schools = schools, 
            workplaces = workplaces))
}

#  Function for reading in pop_table data
read_pop_table <- function(input_dir, folders, data_group) {
  
  pop_table_files <- list.files(paste0(input_dir, "/", folders$pop_table, "/"))
  
  if (data_group == "US") {
    #  For US, should always be households.csv
    pop_table_file <- "households.csv"
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  pop_table <- read.csv(paste0(input_dir, "/", folders$pop_table, "/", pop_table_file), 
                        stringsAsFactors = FALSE)
  return(pop_table)
}


#  Standardize the pop_table
standardize_pop_table <- function(pop_table, data_group){
  if (data_group == "US") {
    pop_table <- data.frame(place_id = pop_table$Id2,
                            n_house = pop_table$NumberOfHouseholds)
  }
  
  return(pop_table)
}


#  Function for reading in pums data
read_pums <- function(input_dir, folders, data_group){
  
  pums_files <- list.files(paste0(input_dir, "/", folders$pums))
  
  if (data_group == "US") {
    
    #  Get rid of any additional folders in the directory
    pums_files <- pums_files[-which(nchar(pums_files) < 5)]
    
    #  Find the indices of the person and household level files
    hp <- substr(pums_files, 5, 5)
    index_h <- which(hp == "h")
    index_p <- which(hp == "p")
    
    #  Read in the person and household level files
    pums_h <- read.csv(paste0(input_dir, "/", folders$pums, "/", pums_files[index_h]), 
                       stringsAsFactors = FALSE)
    pums_p <- read.csv(paste0(input_dir, "/", folders$pums, "/", pums_files[index_p]), 
                       stringsAsFactors = FALSE)
    
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  return(list(pums_h = pums_h, pums_p = pums_p))
}


#  Standardize the pums data 
standardize_pums <- function(pums, data_group){
  if (data_group == "US") {
    names(pums$pums_h)[which(names(pums$pums_h) == "PUMA")] <- "puma_id"
    names(pums$pums_p)[which(names(pums$pums_p) == "PUMA")] <- "puma_id"
  }
  
  return(pums)
}

#  Function for reading in lookup data
read_lookup <- function(input_dir, folders, data_group){
  
  lookup_files <- list.files(paste0(input_dir, "/", folders$lookup))
  
  if (data_group == "US") {
    filename <- "lookup10.csv"
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  #  Read in lookup table
  lookup <- read.csv(paste0(input_dir, "/", folders$lookup, "/", filename), 
                     stringsAsFactors = FALSE)
  return(lookup)
}


#  Standardize the lookup table
standardize_lookup <- function(lookup, data_group){
  if (data_group == "US") {
    #  In the future, maybe take the subset of the lookup table corresponding to the state
    #lookup <- subset(lookup, STATE)
    
    #  Below, we add 100, 1000, and 1000000 and then take the substrings
    #  so that each string has the same length
    new_state_fp <- lookup$STATEFP + 100  
    new_state_fp <- substr(new_state_fp, 2, 3)
    new_county_fp <- lookup$COUNTYFP + 1000
    new_county_fp <- substr(new_county_fp, 2, 4)
    new_tract_ce <- lookup$TRACTCE + 1000000
    new_tract_ce <- substr(new_tract_ce, 2, 7)
    place_id <- paste0(new_state_fp, new_county_fp, new_tract_ce)
    
    lookup <- data.frame(place_id = place_id,
                         puma_id = lookup$PUMA5CE)
  }
  return(lookup)
}

#  Function for reading in shapefiles data
read_shapefiles <- function(input_dir, folders, data_group) {

  shapefiles_files <- list.files(paste0(input_dir, "/", folders$shapefiles))
  
  if (data_group == "US") {
    
    #  Navigate to correct folder
    if (length(shapefiles_files) == 1 & !grepl(pattern = "\\.", x = shapefiles_files)){
      folders$shapefiles <- paste0(folders$shapefiles, "/", shapefiles_files)
      shapefiles_files <- list.files(paste0(input_dir, "/", folders$shapefiles))
    }
    
    #  Read in correct file
    ind_shp <- which(grepl(pattern = "\\.shp", x = shapefiles_files) & 
                        !grepl(pattern = "\\.xml", x = shapefiles_files))
    filename <- shapefiles_files[ind_shp]
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  #  Read in shapefile
  shapefile <- readShapeSpatial(paste0(input_dir, "/", folders$shapefiles, "/", filename))
  return(shapefile)
}


#  Standardize the shapefiles 
standardize_shapefiles <- function(shapefiles, data_group){
  if (data_group == "US") {
    names(shapefiles)[which(names(shapefiles) == "GEOID10")] <- "place_id"
    shapefiles$place_id <- as.numeric(as.character(shapefiles$place_id))
  }
  return(shapefiles)
}


#  Function for reading in schools data
read_schools <- function(input_dir, folders, data_group){

  schools_path <- paste0(input_dir, "/", folders$schools, "/")
  
  if (data_group == "US") {
      
    # Read in public school dataframe  
    public_fn <- paste0(schools_path, "ELSI_2011_public_f.csv")
    public_df <- read.csv(public_fn, stringsAsFactors = FALSE)
    public_df$State.Code = gsub("=", "", public_df$State.Code)

    # Read in private school dataframe 
    private_fn <- paste0(schools_path, "ELSI_2010_private_f.csv")
    private_df <- read.csv(private_fn, stringsAsFactors = FALSE)
    private_df$State.Code = gsub("=", "", private_df$State.Code)
    
    # Combine the public and private schools into a list 
    schools <- list(public = public_df, private = private_df)
    return(schools)
    
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
}


#  Function for reading in workplaces data
read_workplaces <- function(input_dir, folders, data_group) {
  workplace_files<- list.files(paste0(input_dir, "/", folders$workplaces))
  
  if (data_group == "US") {
    
    if (length(workplace_files == 1)) {
      filename <- workplace_files
    } else if (length(workplace_files) > 1) {
      filename <- grep("us.workplaces", workplace_files)[1]
    }
    
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  #  Read in shapefile
  workplaces <- read.csv(paste0(input_dir, "/", folders$workplaces, "/", filename), 
                         stringsAsFactors = FALSE)
  return(workplaces)
}
