#  Sam Ventura and Lee Richardson
#  3 November 2015
#  SPEW -- Synthetic Populations and Ecosystems of the World

read_data <- function(path, folders = list(pop_table = "popTables", 
                                           pums = "pums", 
                                           schools = "schools", 
                                           tables = "tables", 
                                           shapefiles = "tiger", 
                                           workplaces = "workplaces"),
                      filenames = NULL) {
  
  if (is.null(filenames)) {
    #  Read in pop_table data
    pop_table <- read_pop_table(path, folders)
    
    #  Read in pums data
    pums <- read_pums(path, folders)
    
    #  Read in tables data
    tables <- read_tables(path, folders)
    
    #  Read in shapefiles data
    shapefiles <- read_shapefiles(path, folders)
    
    #  Read in schools data, if necessary
    if (folders$schools %in% list.files(path)) {
      schools <- read_schools(path, folders)
    } else {
      schools <- NULL
    }
    
    #  Read in workplaces data, if necessary
    if (folders$workplaces %in% list.files(path)) {
      workplaces <- read_workplaces(path, folders)
    } else {
      workplaces <- NULL
    }
    
  } else if (length(filenames) != length(folders)) {
    stop("Length of file filenames does not equal number of folders")
  } else {
    
  }
}


#  Function for reading in pop_table data
read_pop_table <- function(path, folders) {
  pop_table_files <- list.files(folders$pop_table)
  if (length(pop_table_files) == 1) {
    pop_table_file <- pop_table_files
  } else if ("households.csv" %in% pop_table_files) {
    pop_table_file <- pop_table_files[which(pop_table_files == "households.csv")]
  } else {
    stop("SPEW isn't sure what population table files you want")
  }
  pop_table <- read.csv(paste0(path, "/", folders$pop_table, "/", pop_table_file))
  return(pop_table)
}
