## Testing sample-env-locations.R

context("Private school location assignments")


test_that("Private school location assignments", {

    library(rgdal)
    library(sp)
    ## Load in Delaware data
    data(delaware)
    
    ## Shape
    shapefile <- delaware$shapefiles$shapefile
    ## Number of counties
    n_co <- length(unique(shapefile$COUNTYFP10))

    ## Private schools
    schools <- delaware$schools$private
    n_co_schools <- length(unique(schools$CoNo))

    ## Number of counties should be greater/equal than number of unique counties in the schools
    expect_equal(n_co_schools <= n_co, TRUE)

    ## All counties in private schools df appear in map of state
    expect_equal(sum(as.numeric(as.character(unique(schools$CoNo))) %in%
                     as.numeric(as.character(shapefile$COUNTYFP10))), n_co_schools)


    ############################
    ## Sample  locations
    ###########################
    
    ## Reorder by county
    schools_co <- schools[order(schools$CoNo),]
    ## Get how many schoolsa re in each county
    co_table <- table(schools$CoNo)
    ## Get the coordinates
    coords <- do.call('rbind', lapply(1:length(co_table),
                                      sample_county_loc, co_table, shapefile))

    expect_equal(nrow(coords) == nrow(schools_co), TRUE)
    
    ## Combine the dataframes
    schools_locs <- cbind(schools_co, coords)


    #################
    ## Inside sample_county_loc
    #####################

    ## Test all three counties in DE
    for( ind in 1:length(co_table)){
        ## Look at an individual county
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

        ## Number of points should be equal to the number of schools in the county
        expect_true(nrow(locs_df) == n, TRUE)

        ##  Check whether schools are in the county's boundaries
        bds <- as(county_shape, "SpatialLines")
        pts <- as(bds, "SpatialPoints")
        check <- sp::point.in.polygon(locs_df[,1], locs_df[,2], coordinates(pts)[, 1], coordinates(pts)[, 2])
        
        expect_equal(sum(check ==1), n)
    }

})
