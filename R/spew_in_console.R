## Functions for working with SPEW directly in the console

#' A wrapper function for SPEW to use in console
#'
#' @param pop_table dataframe with rows corresponding to places which 
#' need to have a population generated. The dataframe needs the  columns  with the follow names: "puma_id"
#' and "n_house". 
#' @param shapefile sp class object used for assigning households to 
#' particular locations  
#' @param pums_h dataframe with microdata corresponding to households.  Each row of the dataframe should correspond to one household with characteristics such as household size, household income, etc.
#' @param pums_p dataframe with microdata corresponding to people.  Each row of the dataframe should correspond to one person with characteristics such as household ID, race, age, etc.
#' @param supplementary_data default is NULL.   This is a list of supplementary data which may include schools, workplaces, marginals, or moments.
#' @param sampling_method character string indicating the type of sampling 
#' method to use, defaults to "uniform". Can also be "ipf" with appropriate marginal data or "mm" with appropriate moments data as supplementary data.
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads"
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals from people to household counts. Default: FALSE, assumes
#' the population in 'pop_table'  is the total number of households per region.
#' @param do_parallel  a logical indicating whether we should generate synthetic ecoystems in parallel with the foreach  Default is FALSE.  If true, the required packages of 'foreach' and `doParallel` must be installed.
#' @param noise a numeric value indicating the value of the noise we add to household locations during road-based sampling.  Default is .1.
#' @return a synthetic ecosystem with synthetic households, synthetic individuals along with the place_id and puma_id
#' @export 
spewr <- function(pop_table, shapefile, pums_h, pums_p,
                  supplementary_data = NULL, 
                  sampling_method = "uniform", locations_method = "uniform",
                  convert_count = FALSE, do_parallel = FALSE, noise = .1){

    ## TODO:  Check if data is compatible
    parallel_type <- "SEQ"
    if(do_parallel){
        parallel_type <- "SOCK"
    }

    ## Add roads
    if (!is.null(supplementary_data$roads)){
        shapefile <- list(boundaries = shapefile, roads = supplementary_data$roads)
    }

    supp_data <- get_supp_data(supplementary_data)

    ## TODO:  needs adapted to supplementary data

    pops <- spew(base_dir = NULL, pop_table = pop_table, shapefile = shapefile,
             pums_h = pums_h, pums_p = pums_p,
             schools = supp_data$schools, workplaces = supp_data$workplaces,
             marginals = supp_data$marginals,
             convert_count = convert_count, parallel_type = parallel_type,
             sampling_method = sampling_method, locations_method = locations_method,
             outfile_loc = "", do_subset_pums = TRUE, do_write = FALSE, noise = noise)

    return(pops)
}

#' Format the supplementary data so it is compatible with SPEW
#'
#' @param supplementary_data list of supplementary data
#' @return list of supplementary data that is compatible with SPEW
get_supp_data <- function(supplementary_data){
    supp_data <- list(schools = NULL, workplaces = NULL,
                      marginals = NULL)
    ## Put the data in the proper place
    if(!is.null(supplementary_data)){
        nms <- names(supplementary_data)
        for (ii in 1:length(supplementary_data)) {
            if(nms[ii] %in% names(supp_data)){
                ind <- which(names(supp_data) == nms[i])
                supp_data[[ind]] <- supplementary_data[[ii]]
            }else if (nms[ii] == "moments"){
                supp_data$marginals <- supplementary_data[[ii]]
            }
        }
    }
    return(supp_data)
}

    


#' Plotting the synthetic ecosystem
#'
#' @param input_data a list of the essential data and possibly supplementary data, shapefile must be one of the names
#' @param syneco the outputted synthetic ecosystem data from spewR
#' @param region_name string, will become the title of the plot
#' @param pretty logical if TRUE, we make a ggplot and return it.  Otherwise, we use base R to plot.
#' @return plot of the synthetic ecosystem if applicable
#' @export
plot_syneco <- function(input_data, syneco, region_name = NULL, pretty = FALSE, ...){
    g <- NULL
    if(pretty){
        shp_f <- fortify(input_data$shapefile, region = "place_id")
        g <- ggplot() + geom_polygon(data = shp_f, aes(long, lat, group = group),
                                     fill = "gray60", col = "gray60") +
            ggtitle(paste(region_name, "Synthetic Ecosystem")) +
            theme(line = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  legend.title = element_blank()
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
        if(!is.null(input_data$environments)){
             ## The color blind palette
        cbbPalette <- c("#E69F00", "#56B4E9",
                       "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        cols <- rep(cbbPalette, length.out = length(unique(input_data$environments$Type)))
        colScale <- scale_colour_manual(name = "reg", values = cols)
            g <- g + geom_point(data = input_data$environments, aes(x = longitude, y = latitude,
                                                                    col = Type, group = Type),  shape = 17, size = 5) + colScale
        }
        
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
        if(!is.null(input_data$environments)){
            cbbPalette <- c("#E69F00", "#56B4E9",
                       "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
            points(input_data$environments$longitude, input_data$environments$latitude,
                   pch = 17, col = cbbPalette[factor(input_data$environments$Type)])
        }
        title(paste(region_name, "Synthetic Ecosystem"))
    }
    invisible(g)
}

#' Summarize a syneco generated by SPEW
#'
#' @param syneco the return object from spewr
#' @param vars_df a dataframe with 3 columns with names "var_name", "pop_type", and "var_type".  var_name should correspond to a variable within the syneco.  pop_type is either "hh" or "p" indicating whether the variable is to be summarized at the household level or person level.  var_type is either "cont" for continuous variable or "cat" for categorical variable.  This changes how the variable is summarized.
#' @param make_plots logical indicating whether we want plots of the individual variables.  Currently is not functional.
#' @param verbose if TRUE then print out summaries.  Default is FALSE
#' @return list of output summaries
#' @export
summarize_syneco <- function(syneco, vars_df,
                             make_plots = FALSE, verbose = FALSE){
    ## Extract the indices with a non-zero number of households and subset
    syneco_inds <- sapply(syneco, function(el) class(el) == "list")
    syneco <- syneco[syneco_inds]
    
    ## make dataframes of households and people
    households <- do.call('rbind', lapply(syneco, "[[", 1))
    people <-  do.call('rbind', lapply(syneco, "[[", 2))
    out <- vector(mode = "list", length = nrow(vars_df))

    ## loop through the rows and summarize the variable
    for(rr in 1:nrow(vars_df)){
        out[[rr]] <- summarize_syneco_var(vars_df[rr,, drop = FALSE],
                                          households, people, make_plots = make_plots,
                                          verbose = verbose)
    }
    names(out) <- vars_df$var_name
    invisible(out)
}


#' Summarize a variable
#' 
#' @param df dataframe with one row with var_name, pop_type, and var_type columns
#' @param households synthetic ecosystem of households (dataframe)
#' @param people synthetic ecosystem of people (data frame)
#' @param make_plots.  Default is FALSE.  If TRUE, summarize through plots
#' @param verbose if TRUE then print out summaries.  Default is FALSE
#' @return summary of the particular variable
summarize_syneco_var <- function(df, households, people,
                                 make_plots = FALSE, verbose = FALSE){
    ## summarize either the household df or  people df
    if( df$pop_type == "hh"){
        sum_df <- households
    } else if (df$pop_type == "p"){
        sum_df <- people
    } else{stop("pop_type must either be 'hh' or 'p'")}
    stopifnot(df$var_name %in% names(sum_df))
    if(df$var_type == "cont"){
        out <- ddply(sum_df, .(place_id), function(sum_df) summary(sum_df[, df$var_name]))
    } else if (df$var_type == "cat"){
        factors <- factor(sum_df[, df$var_name])
        out <- ddply(sum_df, .(place_id), function(sum_df){
            vals <- sum_df[, df$var_name]
            return(table(factor(vals, levels = levels(factors))))
        })
            
    } else{stop("var_type must either be 'cont' or 'cat'" ) }
    if(verbose){
        cat(paste("Variable:", df$var_name, "\n Summary:"))
        print(out, row.names = FALSE)
    }
    return(out)
}
