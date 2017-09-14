## SKG
## August 24, 2017
## Functions to analyze SPEW synecos in console

#' Plotting the synthetic ecosystem
#'
#' @param input_data a list of the essential data and possibly supplementary data, shapefile must be one of the names
#' @param syneco the outputted synthetic ecosystem data from spew
#' @param region_name string, will become the title of the plot
#' @param  color_list optional list of colors to provide to the synthetic ecosystem.  This must be a list with the following components, "bds", "interior", "roads", "agents", "envs" where each entry in the list is a color or vector of colors
#' @return plot of the synthetic ecosystem 
#' @export
plot_syneco <- function(input_data, syneco, region_name = NULL,
                        color_list = list(bds = "white",
                                          interior = "gray60",
                                          roads = "gray10",
                                          agents = "darkorchid3",
                                          envs = c("#E69F00", "#56B4E9",
                                                   "#009E73", "#F0E442", "#0072B2",
                                                   "#D55E00", "#CC79A7"))
                                          ){
    ## Fortify the shapefile(s)
    shapefile <- ggplot2::fortify(input_data$shapefile, region = "place_id")
    if(!is.null(input_data$roads)) roads <- ggplot2::fortify(input_data$roads)
    
    ## Plot the interior
    g <- plot_interior(shapefile, color_list = color_list)

    ## Plot the roads
    g <- plot_roads(roads, g,  color_list)

    ## Plot the boundaries
    g <- plot_bds(shapefile, g, color_list)

        
    ## Plot the agents
    g <- plot_agents(syneco, input_data, g, color_list)

    ## Plot the environments
    g <- plot_env(input_data, g, color_list)

    ## Add the title and labels
    g <- plot_labs(region_name, g)

    return(g)
}



#' Plot the interior of the synthetic ecosystem
#'
#' @param shapefile a fortified shapefile object, ready for plotting in ggplot.
#' @param g a ggplot.  Default is NULL.
#' @param color_list optional list of colors to provide to the synthetic ecosystem. This must be a list with the following components,  "bds", "interior", "roads", "agents", "envs" where each entry in the list is a color or vector of colors
#' @return a ggplot of the region
plot_interior <- function(shapefile, g = NULL, 
                     color_list =  list(bds = "white",
                                          interior = "gray60",
                                          roads = "gray10",
                                          agents = "darkorchid3",
                                          envs = c("#E69F00", "#56B4E9",
                                                   "#009E73", "#F0E442", "#0072B2",
                                                   "#D55E00", "#CC79A7"))){

    if(is.null(g))     g <- ggplot2::ggplot() # Set up empty plot

    g <- g + ggplot2::geom_polygon(data = shapefile,
                                   ggplot2::aes_string(x = "long", y = "lat", group = "group"),
                                   fill = color_list$interior, col = color_list$interior)
    return(g)
}



#' Plot the boundaries of the synthetic ecosystem
#'
#' @param shapefile a fortified shapefile object, ready for plotting in ggplot.
#' @param g a ggplot.  Default is NULL.
#' @param color_list optional list of colors to provide to the synthetic ecosystem. This must be a list with the following components,  "bds", "interior", "roads", "agents", "envs" where each entry in the list is a color or vector of colors
#' @return a ggplot of the region
plot_bds <- function(shapefile, g = NULL, 
                     color_list =  list(bds = "white",
                                          interior = "gray60",
                                          roads = "gray10",
                                          agents = "darkorchid3",
                                          envs = c("#E69F00", "#56B4E9",
                                                   "#009E73", "#F0E442", "#0072B2",
                                                   "#D55E00", "#CC79A7"))){

    if(is.null(g))     g <- ggplot2::ggplot() # Set up empty plot
    g <- g + ggplot2::geom_polygon(data = shapefile,
                                   ggplot2::aes_string(x = "long", y = "lat", group = "group"),
                                   col = color_list$bds, lwd = 2, alpha = .4)
    return(g)
}


#' Plot the agents of synthetic ecosystem
#'
#' @param syneco the outputted synthetic ecosystem data from spew
#' @param input_data a list of the essential data and possibly supplementary data, shapefile must be one of the names
#' @param g a ggplot.  Default is NULL.
#' @param color_list optional list of colors to provide to the synthetic ecosystem. This must be a list with the following components:
#' "bds", "interior", "roads", "agents", "envs" where each entry in the list is a color or vector of colors
#' 
#' @return a ggplot of the region
plot_agents <- function(syneco, input_data,
                        g = NULL, 
                        color_list =  list(bds = "white",
                                          interior = "gray60",
                                          roads = "gray10",
                                          agents = "darkorchid3",
                                          envs = c("#E69F00", "#56B4E9",
                                                   "#009E73", "#F0E442", "#0072B2",
                                                   "#D55E00", "#CC79A7"))) {
  
  if (is.null(g)) { g <- ggplot2::ggplot() } # Set up empty plot 
  
  households <- do.call('rbind', lapply(syneco, "[[", 1))
  g <- g + ggplot2::geom_point(data = households,
                               ggplot2::aes_string(x = "longitude", y = "latitude"),
                               alpha = 1, col = color_list$agents)
  coords_df <- data.frame(sp::coordinates(input_data$shapefile),
                          place_id = input_data$shapefile@data$place_id)
  g <- g + ggplot2::geom_text(data = coords_df,
                              ggplot2::aes_string(x = "X1", y = "X2", label = "place_id"),
                              col = color_list$bds, size = 8)
  return(g)
  
}



#' Plot the roads of the synthetic ecosystem
#'
#' @param roads a fortified shapefile object of the roads, ready for plotting in ggplot.
#' @param g a ggplot.  Default is NULL.
#' @param color_list optional list of colors to provide to the synthetic ecosystem. This must be a list with the following components,  "bds", "interior", "roads", "agents", "envs" where each entry in the list is a color or vector of colors
#' @return a ggplot of the region
plot_roads <- function(roads, g = NULL, 
                     color_list =  list(bds = "white",
                                          interior = "gray60",
                                          roads = "gray10",
                                          agents = "darkorchid3",
                                          envs = c("#E69F00", "#56B4E9",
                                                   "#009E73", "#F0E442", "#0072B2",
                                                   "#D55E00", "#CC79A7"))){

    if(is.null(g))     g <- ggplot2::ggplot() # Set up empty plot
     g <- g + ggplot2::geom_path(data = roads, ggplot2::aes_string(x = "long", y = "lat", group = "group"),
                               col = color_list$roads, lwd =2 , alpha = .9)
    return(g)
}

#' Plot the environments of the synthetic ecosystem
#'
#' @param input_data a list of the essential data and possibly supplementary data, shapefile must be one of the names
#' @param g a ggplot.  Default is NULL.
#' @param color_list optional list of colors to provide to the synthetic ecosystem. This must be a list with the following components,  "bds", "interior", "roads", "agents", "envs" where each entry in the list is a color or vector of colors
#' @return a ggplot of the region
plot_env <- function(input_data, g = NULL, 
                     color_list =  list(bds = "white",
                                          interior = "gray60",
                                          roads = "gray10",
                                          agents = "darkorchid3",
                                          envs = c("#E69F00", "#56B4E9",
                                                   "#009E73", "#F0E442", "#0072B2",
                                                   "#D55E00", "#CC79A7"))){

    if (is.null(g)) { g <- ggplot2::ggplot() } # Set up empty plot
    cols <- rep(color_list$envs, length.out = length(unique(input_data$environments$Type)))
    col_scale <- ggplot2::scale_colour_manual(name = "reg", values = cols)
    g <- g + ggplot2::geom_point(data = input_data$environments,
                        ggplot2::aes_string(x = "longitude", y = "latitude", col = "Type", group = "Type"),  
                        shape = 17, size = 5) + col_scale
    return(g)
}

#' Add the labels and the theme to the plot
#'
#' @param region_name string, will become the title of the plot
#' @param g a ggplot.  Default is NULL.
#' 
plot_labs <- function(region_name, g = NULL) {
    if(is.null(g))     g <- ggplot2::ggplot() # Set up empty plot

    g <- g + ggplot2::ggtitle(paste(region_name, "Synthetic Ecosystem")) + 
          ggplot2::theme(axis.line=ggplot2::element_blank(),
              axis.text.x=ggplot2::element_blank(),
              axis.text.y=ggplot2::element_blank(),
              axis.ticks=ggplot2::element_blank(),
              axis.title.x=ggplot2::element_blank(),
              axis.title.y=ggplot2::element_blank(),
              legend.title = ggplot2::element_blank(),
              panel.background=ggplot2::element_blank(),
              panel.border=ggplot2::element_blank(),
              panel.grid.major=ggplot2::element_blank(),
              panel.grid.minor=ggplot2::element_blank(),
              plot.background=ggplot2::element_blank())

    return(g)

}


#' Summarize spew output
#'
#' @param syneco output from the 'spew' function
#' @param vars_to_sum_h character vector of variables from the household data frame output to summarize
#' @param vars_to_sum_p character vector of variables from the person data frame output to summarize
#' @param vars_to_sum_env character vector of variables from the person data frame which correspond to environment assignments.  Default is NULL.
#' @param samp_size number of agents to retain from each lower-level region, for plotting purposes only.  Default is 10^4.
#' @param type Only used when output_dir is specified."US" for a US population, "IPUMS" for IPUMS population, or "custom" for a custom population.  This effects what the summary levels are.  
#' @param summary_level   Only used when output_dir is specified.  IFor the US, 1-state, 2-county, 3-tract.  For IPUMS, 1 -country, 2-province.  For "custom," these are defined by the user's input data.
#' @param marginals list containing all of the marginal totals.  See ?make_ipf_marg for more details.
#' @param output_dir path to top level directory of SPEW folders.  Ex. "./10" for Delaware.  Default is NULL.  In the case it is NULL, we do not need to read in data.
#' @param top_region_id name of the region.  Default is NULL.  It is only used in the case where we directly summarize the syneco object.
#' @param has_marg  Does the region of marginals to refer to?  Logical.  Default is FALSE.
#' @return list with the household summary list, people summary list, header for households, and header 
#' for people, and a data frame of plotting coordinates by summary region
#' 
#' @note This function is only guaranteed to work when you provide marginals describing how a category is "cut."  If a certain category is not represented, then the final totals in each category may be off.
#' 
#' @export
summarize_spew_out <- function(syneco= NULL,
                             vars_to_sum_h, 
                             vars_to_sum_p,
                             vars_to_sum_env=NULL,
                             samp_size=10^4,
                             type = "US",
                             summary_level=2,
                             marginals = NULL,
                             output_dir = NULL,
                             top_region_id = NULL,
                             has_marg = FALSE){

    ## If the output_dir is given, load in the individual files and summarize
    if(!is.null(output_dir)) return(summarize_top_region(output_dir, type, vars_to_sum_h,
                                                         vars_to_sum_p, vars_to_sum_env, samp_size,
                                                         summary_level, marginals))

    ## Otherwise, summarize the outputted spew syneco object
    out <- summarize_syneco(syneco,   vars_to_sum_h, 
                             vars_to_sum_p,
                             vars_to_sum_env=NULL,
                             samp_size=10^4,
                            marginals = NULL,
                            top_region_id)
    return(out)
  
}


#' Summarize synthetic ecosystem for SPEW console output
#' 
#' @param syneco output from the 'spew' function
#' @param vars_to_sum_h character vector of variables from the household data frame output to summarize
#' @param vars_to_sum_p character vector of variables from the person data frame output to summarize
#' @param vars_to_sum_env character vector of variables from the person data frame which correspond to environment assignments.  Default is NULL.
#' @param samp_size number of agents to retain from each lower-level region, for plotting purposes only.  Default is 10^4.
#' @param marginals list containing all of the marginal totals.  See ?make_ipf_marg for more details.
#' @param top_region_id Name supplied of region.
#' @param has_marg  Does the region of marginals to refer to?  Logical.  Default is FALSE.
#' @return list with the household summary list, people summary list, header for households, and header for people, and a data frame of plotting coordinates by summary region
summarize_syneco <- function(syneco,   vars_to_sum_h, 
                             vars_to_sum_p,
                             vars_to_sum_env=NULL,
                             samp_size=10^4,
                             marginals = NULL,
                             top_region_id = "Ecosystem",
                             has_marg = FALSE){

    header_h <- colnames(syneco[[1]]$households)
    header_p <- colnames(syneco[[1]]$people)
    
    ## Loop through each region of SPEW output, summarize output, and aggregate together
    hh_sum_list <- lapply(syneco, summarize_spew_region, type = "households",
                          marginals, vars_to_sum_h, vars_to_sum_env)
    p_sum_list <- lapply(syneco, summarize_spew_region, type = "people",
                         marginals, vars_to_sum_p, vars_to_sum_env)

    hh_sum_list <- hh_sum_list[!sapply(hh_sum_list, is.null)] # get rid of empty regions
    p_sum_list <- p_sum_list[!sapply(p_sum_list, is.null)]

    ## Organize parts into a more organized list
    out_list <- organize_summaries(hh_sum_list, p_sum_list,
                                   header_h, header_p,
                                   vars_to_sum_h, vars_to_sum_p, vars_to_sum_env,
                                   samp_size, top_region_id, coords=FALSE,
                                   has_marg = has_marg)
    
    return(out_list)
    
}

#' Summarize a singular region from spew output
#'
#' @param spew_region a single region (entry in the list) from spew
#' @param type either "households or "people".  Default is "households".  The type to summarize
#' @param marginals marginals object
#' @param vars_to_sum names of the variables to summarize
#' @param vars_to_sum_env environment variables to summarize.  Default is NULL
#' 
#' @return a list of length of  1 for the  region ID , 1 for population size, and optionally 
#' one for a dataframe of stored lat/long coords, the number of variables to summarize , 
#' and the environmental variables
summarize_spew_region <- function(spew_region, type = "households", marginals,
                                  vars_to_sum, vars_to_sum_env = NULL){

    if (class(spew_region) != "list"){
        return(NULL)
    }
    df <- spew_region[type][[1]]

    region_id <- spew_region$place_id

    total_pop <- nrow(df)

    ## Sample coordinates if appropriate
    coords_df <- NULL
    
    ## Align the PUMS to marginals if we have them
    env_df <- df # need to save environment variables for later
    if(!is.null(marginals)){
        marg_vars <- which(names(marginals) %in% vars_to_sum)
        df <- align_pums(df[, vars_to_sum], marginals[marg_vars])
    }
    vars_sum <- summarize_features(df, marginals, vars_to_sum)

    ## Summarize the environment variables
    env_sum <- summarize_environment(env_df, vars_to_sum_env)

    ## Gather everything into a list
    region_summary <- list(region_id = region_id, pop_size = total_pop,
                           coords_df = coords_df)
    
    region_summary <- c(region_summary, vars_sum, env_sum)
    
    return(region_summary)
}
