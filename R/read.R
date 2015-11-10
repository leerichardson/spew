#  Sam Ventura, Lee Richardson, and Shannon Gallagher
#  3 November 2015
#  SPEW -- Synthetic Populations and Ecosystems of the World
#  Update:  11/10/15

read_data <- function(path, folders = list(pop_table = "popTables", 
                                           pums = "pums", 
                                           schools = "schools", 
                                           lookup = "tables", 
                                           shapefiles = "tiger", 
                                           workplaces = "workplaces"),
                      filenames = NULL, data_group = "US") {
  
  if (data_group != "US" & data_group != "ipums")  stop("SPEw doesn't recognize your data_group\nOnly 'US' and 'ipums' are currently supported")
  
  if (is.null(filenames)) {
    #  Read in pop_table data
    pop_table <- read_pop_table(path, folders, data_group)
    
    #  Read in pums data
    pums <- read_pums(path, folders, data_group)
    
    #  Read in lookup data
    lookup <- read_lookup(path, folders, data_group)
    
    #  Read in shapefiles data
    shapefiles <- read_shapefiles(path, folders, data_group)
    
    #  Read in schools data, if necessary
    if (folders$schools %in% list.files(path)) {
      schools <- read_schools(path, folders, data_group)
    } else {
      schools <- NULL
    }
    
    #  Read in workplaces data, if necessary
    if (folders$workplaces %in% list.files(path)) {
      workplaces <- read_workplaces(path, folders, data_group)
    } else {
      workplaces <- NULL
    }
    
  } else if (length(filenames) != length(folders)) {
    stop("Length of file filenames does not equal number of folders")
  } else {
    
  }
  
  return(list(pop_table = popTables, 
              pums = pums, 
              lookup = lookup, 
              shapefiles = tiger, 
              schools = schools, 
              workplaces = workplaces))
}


#  Function for reading in pop_table data
read_pop_table <- function(path, folders, data_group) {
  
  pop_table_files <- list.files(paste0(path, "/", folders$pop_table))
  
  if (data_group == "US") {
    #  For US, should always be households.csv
    pop_table_file <- "households.csv"
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  pop_table <- read.csv(paste0(path, "/", folders$pop_table, "/", pop_table_file))
  return(pop_table)
}


#  Function for reading in pums data
#  Need conditional for each of these two cases:
#  1.  If given both person and household level, return both separately.
#  2.  If given only person level, extract household level, return both separately.
read_pums <- function(path, folders, data_group){
  
  pums_files <- list.files(paste0(path, "/", folders$pums))
  
  if (data_group == "US") {
    
    #  Get rid of any additional folders in the directory
    pums_files <- pums_files[-which(nchar(pums_files) < 5)]
    
    #  Find the indices of the person and household level files
    hp <- substr(pums_files, 5, 5)
    index_h <- which(hp == "h")
    index_p <- which(hp == "p")
    
    #  Read in the person and household level files
    pums_h <- read.csv(paste0(path, "/", folders$pums, "/", pums_files[index_h]))
    pums_p <- read.csv(paste0(path, "/", folders$pums, "/", pums_files[index_p]))
    
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  return(list(pums_h = pums_h, pums_p = pums_p))
}


#  Function for reading in lookup tables data
#  Not sure if we need this right now
#  Possibly add later
read_lookup <- function(path, folders, data_group){
  
  lookup_files <- list.files(paste0(path, "/", folders$lookup))
  
  if (data_group == "US") {
    filename <- "lookup10.csv"
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  #  Read in lookup table
  lookup <- read.csv(paste0(path, "/", folders$lookup, "/", filename))
  return(lookup)
}


#  Function for reading in lookup tables data
read_shapefiles <- function(path, folders, data_group){
  
  shapefiles_files <- list.files(paste0(path, "/", folders$shapefiles))
  
  if (data_group == "US") {
    
    #  Navigate to correct folder
    if(length(shapefiles_files) == 1 & !grepl(pattern = "\\.", x = shapefiles_files)){
      folders$shapefiles <- paste0(folders$shapefiles, "/", shapefiles_files)
      shapefiles_files <- list.files(paste0(path, "/", folders$shapefiles))
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
  shapefile <- readShapeSpatial(paste0(path, folders$shapefiles, "/", filename))
  return(shapefile)
}

