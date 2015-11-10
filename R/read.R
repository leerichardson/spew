#  Sam Ventura and Lee Richardson
#  3 November 2015
#  SPEW -- Synthetic Populations and Ecosystems of the World

read_data <- function(path, folders = list(pop_table = "popTables", 
                                           pums = "pums", 
                                           schools = "schools", 
                                           tables = "tables", 
                                           shapefiles = "tiger", 
                                           workplaces = "workplaces"),
                      filenames = NULL, data_group = "US") {
  
  if (data_group != "US" & data_group != "ipums")  stop("SPEW doesn't recognize your data_group\nOnly 'US' and 'ipums' are currently supported")
  
  if (is.null(filenames)) {
    #  Read in pop_table data
    pop_table <- read_pop_table(path, folders, data_group)
    
    #  Read in pums data
    pums <- read_pums(path, folders, data_group)
    
    #  Read in tables data
    tables <- read_tables(path, folders, data_group)
    
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
}


#  Function for reading in pop_table data
read_pop_table <- function(path, folders, data_group) {
  
  pop_table_files <- list.files(paste0(path, "/", folders$pop_table))
  
  if (data_group == "US") {
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
read_pums <- function(path, folders, data_group){
  
  pums_files <- list.files(paste0(path, "/", folders$pums))
  
  if (data_group == "US") {
    pums_files <- pums_files[-which(nchar(pums_files) < 5)]
    hp <- substr(pums_files, 5)
    index_h <- which(hp == "h")
    index_p <- which(hp == "p")
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
read_tables <- function(path, folders, data_group){
  
  tables_files <- list.files(paste0(path, "/", folders$tables))
  
  if (data_group == "US") {
    #  do stuff
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  return()
}


#  Function for reading in lookup tables data
read_shapefiles <- function(path, folders, data_group){
  
  shapefiles_files <- list.files(paste0(path, "/", folders$shapefiles))
  
  if (data_group == "US") {
    #  do stuff
  } else if (data_group == "ipums") {
    #  do stuff
  } else {
    #  do stuff
  }
  
  return()
}


