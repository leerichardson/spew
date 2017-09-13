## Functions to run the diagnostics
## Revised as of August, 2017 for clarity and simplicity


#' Summarize the region in a more human-readable format
#'
#' @param output_dir path to top level directory of SPEW folders.  Ex. "./10" for Delaware
#' @param type "US" for a US population, "IPUMS" for IPUMS population, or "custom" for a custom population.  This effects what the summary levels are.  
#' @param vars_to_sum_h character vector of variables from the household data frame output to summarize
#' @param vars_to_sum_p character vector of variables from the person data frame output to summarize
#' @param vars_to_sum_env character vector of variables from the person data frame which correspond to environment assignments.  Default is NULL.
#' @param samp_size number of agents to retain from each lower-level region, for plotting purposes only.  Default is 10^4.
#' @param summary_level   For the US, 1-state, 2-county, 3-tract.  For IPUMS, 1 -country, 2-province.  For "custom," these are defined by the user's input data.
#' @param marginals list containing all of the marginal totals.  See ?make_ipf_marg for more details.
#' @return list with the household summary list, people summary list, header for households, and header for people, and a data frame of plotting coordinates by summary region
#' @note This function is only guaranteed to work when you provide marginals describing how a category is "cut."  If a certain category is not represented, then the final totals in each category may be off.
summarize_top_region <- function(output_dir,
                                 type="US",
                                 vars_to_sum_h, 
                                 vars_to_sum_p,
                                 vars_to_sum_env=NULL,
                                 samp_size=10^4,
                                 summary_level=2,
                                 marginals = NULL){

    ## Make sure output directory is in right structure
    stopifnot(check_path(output_dir))
    top_region_id <- basename(output_dir)
    
    ## Get the headers 
    header_h <- get_header(output_dir, type = "household")
    header_p <- get_header(output_dir, type = "people")

    ## Get the files to read, separated by summary level
    ## Will get the "household" files specifically
    household_filenames <- get_filenames(output_dir, summary_level,
                                         agent_type = "household",
                                         pop_type = type)
    people_filenames <- get_filenames(output_dir, summary_level,
                                         agent_type = "people",
                                         pop_type = type)

    ## Check that the variable names match those in the files
    stopifnot(check_var_names(header_h, header_p,
                              vars_to_sum_h, vars_to_sum_p,
                              vars_to_sum_env))

    ## Summarize individual levels
    hh_sum_list <- lapply(household_filenames, summarize_spew,
                          marginals, vars_to_sum_h,
                          coords = FALSE, read = TRUE)
    p_sum_list <- lapply(people_filenames, summarize_spew,
                         marginals, vars_to_sum_p, vars_to_sum_env,
                         samp_size, coords=TRUE, read = TRUE)

    hh_sum_list <- hh_sum_list[!sapply(hh_sum_list, is.null)] # get rid of empty regions
    p_sum_list <- p_sum_list[!sapply(p_sum_list, is.null)]

    ## Organize parts into a more organized list
    out_list <- organize_summaries(hh_sum_list, p_sum_list,
                                   header_h, header_p,
                                   vars_to_sum_h, vars_to_sum_p, vars_to_sum_env,
                                   samp_size, top_region_id, coords=TRUE,
                                   has_marg = TRUE)

    return(out_list)
}
                                 
#' Check the path to output to run diags
#'
#' @param output_dir directory to TOP level region
#' @return logical
check_path <-function(output_dir){
    files <- list.files(output_dir, recursive = TRUE)
    n_files <- length(grep("household.*.csv", files))
    if(n_files < 1) return(FALSE)
    return(TRUE)
}

#' Extract the header from a population
#'
#' @param output_dir directory to TOP level region
#' @param type either "household" or "people"
#' @return a character vector of the header
get_header <- function(output_dir, type = "household"){

    output_dir <- file.path(output_dir, "output")
    files <- list.files(output_dir,  recursive = TRUE)
    file_ind <- grep(paste0(type, ".*.csv"), files)[1]
    header <- read.csv(file.path(output_dir, files[file_ind]), nrows = 1, header = FALSE,
                       stringsAsFactors = FALSE)
    header <- unlist(header)
    names(header) <- NULL
    return(header)
}

#' Get the filenames of the SPEW output, separated by the level
#' 
#' @param output_dir path to top level directory of SPEW folders.  Ex. "./10" for Delaware
#' @param summary_level   For the US, 1-state, 2-county, 3-tract.  For IPUMS, 1 -country, 2-province.  For "custom," these are defined by the user's input data.
#' @param agent_type either "household" or "people"
#' @param pop_type "US" for a US population, "IPUMS" for IPUMS population, or "custom" for a custom population.  This effects what the summary levels represent
#' @return list of lists where the first list is a list of the different filenames for each summary level and the second list contains a dataframe of the different file paths along with the name of the larger, aggregated region
get_filenames <- function(output_dir, summary_level=2,
                          agent_type = "household", pop_type = "US"){

    output_dir <- file.path(output_dir, "output")
    files <- list.files(output_dir, recursive = TRUE)
    
    files <- grep(paste0(agent_type, ".*.csv"), files, value = TRUE)
    if(pop_type == "US"){
        ## Get a data frame of corresponding state, county, and tract
        st_co_tr <- extract_st_co_tr(files)
        df <- data.frame(files = files, st_co_tr)
        vars <- c("state", "county", "tract")[1:summary_level]
        file_list <- plyr::dlply(df, .variables = vars, .fun = function(df){
            list(files = file.path(output_dir, as.character(df$files)), id = paste(df[1, vars], collapse = ""))
        })
    } else {
        names <- basename(files)
        province <- gsub(paste0(agent_type, "_"), "", names)
        province <- gsub(".csv", "", province)
        df <- data.frame(files = files, province = province)
        file_list <- plyr::dlply(df, .variables = "province", .fun = function(df){
            list(files = file.path(output_dir, as.character(df$files)),
                 id = as.character(df[1, 2]))
        })
    }
    return(file_list)

}


#' Extract the state, county, and tract ID from a string
#'
#' @param files character vector of the files
#' @return a data frame with the state, county, and tract IDs, along with the aggregated ID
extract_st_co_tr <- function(files){
    files <- basename(files)
    stcotr <- gsub("[^0-9]", "", files)
    stopifnot(all(nchar(stcotr) == 11))
    st <- substr(stcotr, 1, 2)
    co <- substr(stcotr, 3, 5)
    tr <- substr(stcotr, 6, 11)
    df <- data.frame(state = st, county = co, tract = tr, stcotr= stcotr,
                     stringsAsFactors = FALSE)
    return(df)
}
 
#' Check to see if variable names are in SPEW outputs
#'
#' @param header_h character vector of household variables in household SPEW output
#' @param header_p character vector of people variables in household SPEW output
#' @param vars_to_sum_h character vector of household variables we want to summarize
#' @param vars_to_sum_p character vector of people variables we want to summarize
#' @param vars_to_sum_env character vector of variables from the person data frame which correspond to environment assignments.  Default is NULL
#' @return logical
check_var_names <- function(header_h, header_p,
                            vars_to_sum_h, vars_to_sum_p,
                            vars_to_sum_env = NULL){
    hh <- all(vars_to_sum_h %in% header_h)
    p <- all(vars_to_sum_p %in% header_p)
    env <- TRUE
    if(!is.null(env)) env <- all(vars_to_sum_env %in% header_p)
    
    return(hh & p & env)
}


#' Summarize a SPEW region
#'
#' @param filenames list of full path to files to summarize and the id of the region
#' @param marginals list containing all of the marginal totals.  See ?make_ipf_marg for more details.  Default is NULL
#' @param vars_to_sum variables we wish to summarize, should correspond to marginals object
#' @param env_vars environment variables to summarize.  Default is NULL
#' @param coords logical of whether to extract the longitude/latitude coordinates and store
#' @param samp_size number of lon/lat coordinates to sample.  Default is 10^4
#' @param read logical of whether we need to read in the populations.  Default is FALSE
#' @param pops list of the populations produced by SPEW for a household OR people
#' @param type either NULL, "households", or "people".  
#' @return a list of length of  1 for the  region ID , 1 for population size, and optionally one for a dataframe of stored lat/long coords, the number of variables to summarize , and the environmental variables
summarize_spew <- function(filenames, marginals= NULL, vars_to_sum,
                           env_vars=NULL,
                            coords = TRUE, samp_size=10^4,
                           read = FALSE, pops = NULL,
                           type = NULL){
    
    ## Read in the files with relevant columns and only and combine into a large data frame
    read_vars <- vars_to_sum
    if(coords){ read_vars <- c(read_vars, "longitude", "latitude")}
    if(!is.null(env_vars)) read_vars <- c(read_vars, env_vars)
    if("school_id" %in% env_vars){ # a little hacky, but school_id needs to be a character
        df <- as.data.frame(do.call('rbind',
                                    lapply(filenames$files, 
                                           data.table::fread, select = read_vars,
                                           colClasses = list(character = "school_id")
                                           )))
        
    } else{
        df <- as.data.frame(do.call('rbind',
                                    lapply(filenames$files, 
                                           data.table::fread,  select = read_vars)
                                    ))
    }
    region_id <- filenames$id

    

    total_pop <- nrow(df)



    ## Sample coordinates if appropriate
    coords_df <- NULL
    if(coords){
        N <- min(c(total_pop, samp_size), na.rm = TRUE)
        inds <- sample(1:nrow(df), N)
        coords_df <- data.frame(df[inds, c("longitude", "latitude")], region_id = region_id)
    }
    
    ## Align the PUMS to marginals if we have them
    env_df <- df # need to save environment variables for later
    if(!is.null(marginals)){
        marg_vars <- which(names(marginals) %in% vars_to_sum)
        df <- align_pums(df[, vars_to_sum], marginals[marg_vars])
    }
    vars_sum <- summarize_features(df, marginals, vars_to_sum)

    ## Summarize the environment variables
    env_sum <- summarize_environment(env_df, env_vars)

    ## Gather everything into a list
    region_summary <- list(region_id = region_id, pop_size = total_pop,
                           coords_df = coords_df)
    region_summary <- c(region_summary, vars_sum, env_sum)
    return(region_summary)
                           
}

#' Summarize individual features of a region
#'
#' @param df data frame of individuals in region subsetted to vars to sum and marginal equivalents
#' @param marginals marginal list or NULL
#' @param vars_to_sum name of variables to summarize
#' @return list of table of each feature
summarize_features <- function(df, marginals, vars_to_sum){
    if (!is.null(marginals)){
        marg_var_inds <- vars_to_sum %in% names(marginals)
        ## Want to summarize both marginal versions and non marginal if it is not available
        vars_to_sum <- c(vars_to_sum[!marg_var_inds],
                         paste0(vars_to_sum[marg_var_inds], "_marg"))
    }
    features_table <- lapply(1:length(vars_to_sum), function(ind){
        var <- vars_to_sum[ind]
        table(df[, var])
    })
    names(features_table) <- gsub("_marg", "", vars_to_sum)

    return(features_table)
 }     


#' Return the unique environment assignments in a region
#'
#' @param df the dataframe for the region
#' @param env_vars the environmental variables we want to summarize
#' @return list of unique environment assignments for each environment
summarize_environment <- function(df, env_vars){
    if(is.null(env_vars)) return(NULL)
    out <- lapply(env_vars, function(var){
        unique(na.omit(df[, var]))
    })
    names(out) <- env_vars
    return(out)
}



#' Organize the summaries into a more palatable format
#'
#' @param hh_sum_list output of summarize_spew for regions for households
#' @param p_sum_list output of summarize_spew for regions for people
#' @param header_h character vector of all variables in SPEW households
#' @param header_p character vector of all variables in SPEW people
#' @param vars_to_sum_h character vector of household variables we want to summarize
#' @param vars_to_sum_p character vector of people variables we want to summarize
#' @param env_vars variables to summarize
#' @param samp_size total number sampled in each region for plotting
#' @param top_region_id main region's ID or name
#' @param coords logical of whether to extract the longitude/latitude coordinates and store.  Default is TRUE
#' @param has_marg logical.  Does the synthetic ecosystem have marginals to look at?  Default is FALSE
#' @return list including the top region name, headers for households and people, a coordinate plotting data frame scaled to the density of the population with max samp_size records, and data frames of tables for each region for each of the characteristics
organize_summaries <- function(hh_sum_list, p_sum_list,
                               header_h, header_p,
                               vars_to_sum_h, vars_to_sum_p, env_vars,
                               samp_size, top_region_id, coords=TRUE,
                               has_marg = FALSE){

    pop_totals <- get_pop_totals(hh_sum_list, p_sum_list)
    n_house <- sum(pop_totals$n_house)
    n_people <- sum(pop_totals$n_people)
    household_dfs <- get_dfs(hh_sum_list, vars_to_sum_h, has_marg)
    people_dfs <- get_dfs(p_sum_list, vars_to_sum_p, has_marg)
    env_dfs <- get_envs(p_sum_list, env_vars)
    coords_df <- get_coords_scaled(p_sum_list, samp_size,
                                   coords, pop_totals)

    base_list <- list(top_region_id = top_region_id,
                      header_h = header_h, header_p = header_p,
                      n_house = n_house, n_people = n_people,
                      pop_totals = pop_totals, coords_df = coords_df)
    out <- c(base_list, household_dfs, people_dfs, env_dfs)
    return(out)
                     
}

#' Get the population totals from a summarized SPEW region
#'
#' @param hh_sum_list output from summarize_spew for households
#' @param p_sum_list output from summarize_spew for people
#' @return list of a data frame of population totals, one record for each region, number of houses, and number of people
get_pop_totals <- function(hh_sum_list, p_sum_list){
    region <- sapply(hh_sum_list, "[[", "region_id")
    n_house <- sapply(hh_sum_list, "[[", "pop_size")
    n_people <- sapply(p_sum_list, "[[", "pop_size")
    df <- data.frame(region = region, n_house = n_house, n_people = n_people,
                     stringsAsFactors = FALSE)
    rownames(df) <- NULL
    return(df)
}

#' Get the dataframes from SPEW summary output
#'
#' @param sum_list from summarize_spew
#' @param vars_to_sum variables to make data frames of
#' @param has_marg logical.  Does the synthetic ecosystem have marginals to look at?  Default is FALSE
#' @return list of data frame for each variable
#' @details the output is very gg-verse friendly
get_dfs <- function(sum_list, vars_to_sum, has_marg = FALSE){
    region <- as.vector(sapply(sum_list, "[[", "region_id"))
    if(!has_marg){  ## Uses the Reduce function which an result in a stack overflow
        df_list <- lapply(vars_to_sum,
                          function(var){
                              ll <- lapply(sum_list, "[[", var)
                              out_list <- lapply(ll, function(vec){
                                  df <- as.data.frame(matrix(as.numeric(vec), nrow = 1))
                                  colnames(df) <- names(vec)
                                  df
                              })
                              out_list <- lapply(1:length(out_list), function(ind){ # to keep track of the proper region
                                  out_list[[ind]]$region <- region[ind]
                                  out_list[[ind]]
                              })
                              df <- Reduce(merge_reduce, out_list)
                              reg <- which(colnames(df) == "place_id")
                              if(length(reg) > 0) colnames(df)[reg] <- "region"
                              df
                          })
    } else {
        df_list <- lapply(vars_to_sum,
                      function(var){
                          df <- as.data.frame(do.call('rbind',
                                             lapply(sum_list, "[[", var)
                                             ), stringsAsFactors = FALSE)
                          df$region <- region
                          rownames(df) <- NULL
                          df
                      })
    }
    names(df_list) <- vars_to_sum
    return(df_list)
}

#' Wrapper function for merge
#'
#' @param x x component of merge
#' @param y y component of merge
#' @return a function
#' 
#' @note This sets the merge default to all = TRUE.  To use in the Reduce function
merge_reduce <- function(x,y){
    g <- merge(x,y, all = TRUE)
    return(g)
}



#' Gather the unique assignments for the region
#'
#' @param p_sum_list output from summarize_spew
#' @param env_vars the environment variables we want to summarize.  Default is NULL
#' @return a list of unique assignments for each of the env_variables
get_envs <- function(p_sum_list, env_vars=NULL){
    if(is.null(env_vars)) return(NULL)
    envs_list <- lapply(env_vars, function(var){
        envs <- sort(unique(do.call('c', sapply(p_sum_list, "[[", var))))
    })
    names(envs_list) <- env_vars
    return(envs_list)
}

#' Getting a plotting data frame
#'
#' @param sum_list output for people from summarize_spew
#' @param samp_size how many records to keep for plotting
#' @param coords logical
#' @param pop_totals number of people to sample from each region
#' @return data frame for plotting, longitude and latitude coordinates along with region
get_coords_scaled <- function(sum_list, samp_size, coords, pop_totals){
    ## Get a proportional number of individuals from each region
    if(!coords) return(NULL)
    samps <- min(samp_size, max(pop_totals$n_people))
    n_records <- round(samp_size * pop_totals$n_people / sum(pop_totals$n_people))
    n_records <- ifelse(n_records == 0, 1, n_records) # want to show at least one record
    coords_df <- do.call('rbind', lapply(1:length(sum_list), function(ind){
        ll <- sum_list[[ind]]
        region <- ll$region_id
        df <- ll$coords_df
        sample_inds <- sample(1:nrow(df), n_records[ind], replace = TRUE)
        df_sub <- df[sample_inds, ]
        df_sub$longitude <- as.numeric(df_sub$longitude)
        df_sub$latitude <- as.numeric(df_sub$latitude)
        rownames(df_sub) <- NULL
        return(df_sub)
    }))

}


#' Plot SPEW region
#'
#' @param coords_df data frame with "longitude", "latitude", and "region_id" columns
#' @param get_world_map logical.  Get an underlying map from ggmap.  Default is TRUE
#' @param f border size between 0 and 1.  Default is .1.
#' @param region_colors a string of colors to color the map.  Default is from the colorblind friendly palette.
#' @return a ggmap object
plot_region <- function(coords_df, get_world_map = FALSE,    f = .1,
                          region_colors= c("#999999", "#E69F00", "#56B4E9",
                                           "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")){

    ## Get an underlying map if applicable
    g <- get_base_map(coords_df, get_world_map = get_world_map, f=f)
  
    ## Configure the colors
    n_regions <- length(unique(coords_df$region))
    cols <- rep(region_colors, length.out = n_regions)
    col_scale <- ggplot2::scale_colour_manual(name = "region_id", values = cols)

    ## Actually plot the map
    g <- g + ggplot2::geom_point(data = coords_df,
                                 ggplot2::aes(x = longitude, y = latitude, colour = factor(region_id)),
                                 cex = .4) +
        col_scale +
        base_map_theme()
   
    ##  Add the region names
    centers_df <- get_centers(coords_df)
    g <- g + ggplot2::geom_text(data = centers_df,
                       ggplot2::aes(x = longitude, y = latitude, label = region_id,
                           size = 3))

    ## Display map
    print(g)
    return(g)
}

#' Get the base map for plotting
#'
#' @param coords_df data frame with "longitude", "latitude", and "region_id" columns
#' @param get_world_map logical.  Get an underlying map from ggmap.  Default is TRUE
#' @param f border size of map
#' @return a ggplotting object
get_base_map <- function(coords_df, get_world_map = TRUE,
                         f = .1){
    if(get_world_map){
        bbox <- ggmap::make_bbox(coords_df$longitude, coords_df$latitude, f= f)
        map <- tryCatch({ggmap::get_map(bbox, maptype = "toner-lite")},
                        error = function(e) {
                            NULL
                        })
        if(is.null(map)) {
            print("The map is too large to print")
            return(map)}
        g <- ggmap::ggmap(map)
    } else{
        g <- ggplot2::ggplot()
    }
    return(g)

}

#' Get the center longitude and latitude for each region
#'
#' @param coords_df data
#' @param coords_df data frame with "longitude", "latitude", and "region_id" columns
#' @return data frame with mean lon/lat and region
get_centers <- function(coords_df){
    centers_df <- plyr::ddply(coords_df, .variables = "region_id",
                              .fun = function(df){
                                  colMeans(df[, c("longitude", "latitude")])
                              })
    return(centers_df)
}

#' The base map theme for SPEW
#'
#' @return base map theme
base_map_theme <- function(){
    g <-  ggplot2::theme(axis.line=ggplot2::element_blank(),
              axis.text.x=ggplot2::element_blank(),
              axis.text.y=ggplot2::element_blank(),
              axis.ticks=ggplot2::element_blank(),
              axis.title.x=ggplot2::element_blank(),
              axis.title.y=ggplot2::element_blank(),
              legend.position = "none",
              panel.background=ggplot2::element_blank(),
              panel.border=ggplot2::element_blank(),
              panel.grid.major=ggplot2::element_blank(),
              panel.grid.minor=ggplot2::element_blank(),
              plot.background=ggplot2::element_blank())
    return(g)
}

#' Plot characteristic summary output from summarize_top_spew_region
#'
#' @param feature_name string which will be in the title
#' @param legend_name name of the legend title
#' @param feature_df features summary, output from summarize_top_region()
#' @param category_names optional labels to display as the category types.  Default is whatever is contained in the feature_df.
#' @param text_size axis text size.  Default is 10
#' @param region_colors a string of colors to color the map.  Default is from the colorblind friendly palette.
#' @return a gg map object
#' @export
plot_characteristic_proportions <- function(feature_name = "Feature",
                                            legend_name = "Types", feature_df,
                                            category_names = NULL,
                                            text_size = 10,
                                            region_colors= c("#999999", "#E69F00", "#56B4E9",
                                                             "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                                                             ){

    labs <- names(feature_df)
    ind <- which(labs == "region")
    labs <- labs[!grepl("^region$", labs)]
    n_cats <- length(labs)
    if(!is.null(category_names)) labs <- category_names
    stopifnot(length(labs) == n_cats)

    ## Change the raw numbers into proportions
    plot_df <- data.frame(region = feature_df$region)
    plot_df <- data.frame(plot_df, feature_df[, -ind] / rowSums(feature_df[, -ind]))

    ## Color configuration
    cols <- rep(region_colors, length.out = n_cats)
    col_scale <- ggplot2::scale_fill_manual( values = rev(cols) )

    ## Making our df of tables ggplot compatible
    df_melt <- reshape2::melt(plot_df, id.vars = "region")
    colnames(df_melt)[2:3] <- c("feature_name", "Percentage")
    df_melt[, "feature_name"] <- factor( df_melt[, "feature_name"] ,
                                      levels = rev(levels(df_melt[, "feature_name"])),
                                      labels = rev(labs))
    

    ## Plotting the inverted bar chart
    g <- ggplot2::ggplot(df_melt, ggplot2::aes(x = region, y = Percentage,
                                                  fill = feature_name)) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() +
        ggplot2::ggtitle(paste("Ratio of", feature_name, "per region")) + col_scale +
        ggplot2::theme_light() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(text_size)) +
        ggplot2::labs(x = "Region", fill = feature_name)



    print(g)
    return(g)
    
                                                     
}

#' Plot characteristic summary output from summarize_top_spew_region as totals
#'
#' @param feature_df features summary, output from summarize_top_region()
#' @param type either "n_house" or "n_people".  Default is n_people 
#' @param text_size axis text size.  Default is 10
#' @param region_colors a string of colors to color the map.  Default is from the colorblind friendly palette.
#' @return a gg map object
#' @export
plot_pop_totals <- function( feature_df, type = "n_people",
                            text_size = 10,
                            region_colors= c("#999999", "#E69F00", "#56B4E9",
                                             "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")){

    stopifnot(type %in% c("n_people", "n_house"))
    nm <- "Person Counts"
    if(type == "n_house") nm <- "Household Counts"

    ## rename the plotting variable
    ind <- which(names(feature_df) == type)
    colnames(feature_df)[ind] <- "val"

    ## Colors
    cols <- rep(region_colors, length.out = nrow(feature_df))
    col_scale <- ggplot2::scale_fill_manual(values = cols)

    ## Plot
    g <- ggplot2::ggplot(feature_df, ggplot2::aes(x = region, y = val,
                                            fill = region)) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::ggtitle(nm) +
        ggplot2::labs(x = "Region", y = "Totals") + col_scale +
        ggplot2::theme_light() +
        ggplot2:: theme(axis.text.x = ggplot2::element_text(angle = 90, size = text_size),
                        legend.position = "none")
    print(g)
    return(g)
    

}



#' Translate FIPS number to place name
#'
#' @param fips string -- US FIPS code length 2 for state or length 5 for county
#' @param level either "state" or "county"
#' @param df a data frame table to translate FIPS to placename.  It must have column names
#' STATEFP, STATE_NAME, COUNTYFP, and County.
#' @return the placenames corresponding to each FIPS #
fips_to_name<- function(fips, level, df = us){
 #   print("FIPS code is")
 #   print(fips)
    stopifnot(level %in% c("state", "county"))
    stopifnot(!is.null(df))
    if (level == "state"){
        ind <- which(as.character(df$STATEFP) == fips)
        state <- df$STATE_NAME[ind][1]
        return(state)
    } else {
        stopifnot(nchar(fips) == 5)
        ind <- which(as.character(df$COUNTYFP) == fips)
        pn <- df$County[ind][1]
        pn <- gsub(" County", "", pn)
        return(pn)
    }
}
