## May 9, 2017
## Functions to sample locations for environments (private schools, workplaces, and more)




#' Sample private school locations in the output/environments/private_schools.csv file
#'
#' @param region_path path to state, not including output/environments/private_schools.csv
#' @return TRUE and a written file of private_schools_locs.csv with the additional variables of Long and Lat
sample_priv_school_locations <- function(region_path){

    ## Read in schools

    ## Read in shapefile
    shapefile_path <- file.path(region_path, "input/natstat/2010/tract/")
    shapefiles_files <- list.files(shapefile_path)
    ind_shp <- which(grepl(pattern = "\\.shp", x = shapefiles_files) & 
                       !grepl(pattern = "\\.xml", x = shapefiles_files)) 
    filename <- shapefiles_files[ind_shp]
    full_path <- file.path(region_path, "/", filename)
    shapefile <- maptools::readShapeSpatial(full_path)

}




## Testing

region_path <- "~/Desktop/10"
