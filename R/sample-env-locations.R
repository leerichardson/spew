## May 9, 2017
## Functions to sample locations for environments (private schools, workplaces, and more)




#' Sample private school locations in the output/environments/private_schools.csv file
#'
#' @param region_path path to state, not including output/environments/private_schools.csv
#' @return TRUE and a written file of private_schools_locs.csv with the additional variables of Long and Lat
sample_priv_school_locations <- function(region_path){

    ## Read in schools

    ## Read in shapefile
    shapefile_path <- file.path(region_path, "input/shapefiles/natstat/2010/tract")
    shapefiles_files <- list.files(shapefile_path)
    ind_shp <- which(grepl(pattern = "\\.shp", x = shapefiles_files) & 
                       !grepl(pattern = "\\.xml", x = shapefiles_files)) 
    filename <- shapefiles_files[ind_shp]
    filename <- gsub(".shp", "", filename)
    full_path <- file.path(region_path, "input/shapefiles/natstat/2010/tract")
    shapefile <- rgdal::readOGR(".", filename)
    shapefile_coords <- coordinates(shapefile)

    ## Read in schools
    schools <- read.csv(file.path(region_path,
                                  "output/environments/private_schools.csv"))
    ## Reorder by county
    schools_co <- schools[order(schools$CoNo),]

    ## Get how many schoolsa re in each county
    co_table <- table(schools$CoNo)

    ## Get the coordinates
    coords <- do.call('rbind', lapply(1:length(co_table),
                                      sample_county_loc, co_table, shapefile))

    ## Combine the dataframes
    schools_locs <- cbind(schools_co, coords)
     
    ## Write out the file
    write.csv(schools_locs, file.path(region_path,
                                      "output/environments/private_schools_locs.csv"))
    return(TRUE)
}


#' Sample a location within the county for private schools
#'
#' @param ind index
#' @param co_table a table with the names as the county numbers and the values as the number of schools in that county
#' @param shapefile tract shapefile of the state
#' @return data frame of coordinates "Long" and "Lat"
sample_county_loc <- function(ind, co_table, shapefile){
    county <- as.numeric(names(co_table)[ind])
    n <- as.vector(co_table[ind])
    
    ## Extract the tracts in this county and combine into one shapefile
    shape_inds <- which(as.numeric(as.character(shapefile$COUNTYFP10)) == county
                        & shapefile$ALAND10 != 0 )
    ids <- rep(1, length(shape_inds))
    county_shape <- maptools::unionSpatialPolygons(shapefile[shape_inds,], ids)

    ## Sample the coordinates and return as a data frame of Long/Lat
    if (n < 100000){
        locs <- sp::spsample(county_shape, n =n, offset = c(0,0), type = "random", iter = 50)
    } else {
        stop("more than 100,000 private schoools in this county")
    }
    locs_df <- coordinates(locs)
    colnames(locs_df) <- c("Long", "Lat")
    return(locs_df)
}


