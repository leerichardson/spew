## Functions for working with SPEW directly in the console

#' A wrapper function for SPEW to use in console
#'
#' @param pop_table dataframe with rows corresponding to places which 
#' need to have a population generated. Columns also required are "puma_id"
#' and "n_house". 
#' @param shapefile sp class object used for assigning households to 
#' particular locations  
#' @param pums_h dataframe with microdata corresponding to housegolds 
#' @param pums_p dataframe with microdata corresponding to people
#' @param supplementary_data default NULL but otherwise a list of supplementary data which may include schools, workplaces, marginals, or moments.
#' @param sampling_method character vector indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data or "mm" with appropriate moments data. 
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads"
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population is the total number of households
#' @param do_parallel should we run this in parallel with the foreach?  Default is FALSE.  If true, the required packages of 'foreach' and `doParallel` must be installed.
#' @return a synthetic ecosystem with synthetic households, synthetic individuals along with the place_id and puma_id
#' @export 
spewr <- function(pop_table, shapefile, pums_h, pums_p,
                  supplementary_data = NULL, 
                  sampling_method = "uniform", locations_method = "uniform",
                  convert_count = FALSE, do_parallel = FALSE){

    ## TODO:  Check if data is compatible
    
    parallel_type <- "SEQ"
    if(do_parallel){
        parallel_type <- "SOCK"
    }
    
    if(is.null(supplementary_data)){
        schools <- NULL
        workplaces <- NULL
        marginals <- NULL
    }

    ## TODO:  needs adapted to supplementary data

    pops <- spew(base_dir = NULL, pop_table = pop_table, shapefile = shapefile,
             pums_h = pums_h, pums_p = pums_p,
             schools = NULL, workplaces = NULL,
             marginals = NULL, convert_count = FALSE, parallel_type = parallel_type,
             sampling_method = sampling_method, locations_method = locations_method,
             outfile_loc = "", do_subset_pums = TRUE, do_write = FALSE)

    return(pops)
}


#' Plotting the synthetic ecosystem
#'
#' @param input_data a list of the essential data and possibly supplementary data, shapefile must be one of the names
#' @param syneco the outputted synthetic ecosystem data from spewR
#' @param region_name string
#' @param pretty logical if TRUE, we make a ggplot and return it
#' @return plot of the synthetic ecosystem
#' @export
plot_syneco <- function(input_data, syneco, region_name = NULL, pretty = FALSE, ...){
    g <- NULL
    if(pretty){
        shp_f <- fortify(input_data$shapefile, region = "place_id")
        g <- ggplot() + geom_polygon(data = shp_f, aes(long, lat, group = group),
                                     fill = "gray60", col = "gray60") +
            ggtitle(paste(region_name, "Synthetic Ecosystem")) +
            theme(line = element_blank(),
                  panel.background = element_rect(fill = "white")
                  ) + labs(x = "Longitude", y = "Latitude")

        if(!is.null(input_data$roads)){  ## add the roads if available
            roads <- fortify(input_data$roads)
            g <- g + geom_path( data = roads, aes(x=long, y = lat, group = group),
                               col = "gray10", lwd =2 , alpha = .9)
        }
        g <- g +  geom_path(data = shp_f, aes(long, lat, group = group),
                            col = "white", alpha = .4, lwd = 2)
        ## households locations
        households <- do.call('rbind', lapply(syneco, "[[", 1))
        g <- g + geom_point(data = households, aes(longitude, latitude, col = "Household"),
                            col = "darkorchid3", alpha = .95)
        coords_df <- data.frame(coordinates(input_data$shapefile),
                                place_id = input_data$shapefile@data$place_id)
        g <- g + geom_text(data = coords_df, aes(x= X1, y = X2, label = place_id),
                           col = "white", size = 8)
        print(g)
    } else{
        plot(input_data$shapefile)
        if(!is.null(input_data$roads)){
            lines(input_data$roads, col = 4)
        }
        coords <- coordinates(input_data$shapefile)
        text(coords, labels = input_data$shapefile@data$place_id)
        households <- do.call('rbind', lapply(syneco, "[[", 1))
        points(households$longitude, households$latitude, col = 2, pch = 16)
        title(paste(region_name, "Synthetic Ecosystem"))
    }
    invisible(g)
}
