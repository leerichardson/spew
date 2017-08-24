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
    if(!is.null(input_data$roads)) roads <- fortify(input_data$roads)
    
    ## Plot the interior
    g <- plot_interior(shapefile, color_list = color_list)
    
    ## Plot the agents
    g <- plot_agents(syneco, input_data, g, color_list)

    ## Plot the roads
    g <- plot_roads(roads, g,  color_list)

    ## Plot the boundaries
    g <- plot_bds(shapefile, g, color_list)

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
                                   ggplot2::aes (x = long, y = lat, group = group),
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
                                   ggplot2::aes (x = long, y = lat, group = group),
                                   col = color_list$bds, lwd = 2, alpha = .4)
    return(g)
}


#' Plot the agentsof the synthetic ecosystem
#'
#' @param syneco the outputted synthetic ecosystem data from spew
#' @param input_data a list of the essential data and possibly supplementary data, shapefile must be one of the names
#' @param g a ggplot.  Default is NULL.
#' @param color_list optional list of colors to provide to the synthetic ecosystem. This must be a list with the following components,  "bds", "interior", "roads", "agents", "envs" where each entry in the list is a color or vector of colors
#' @return a ggplot of the region
plot_agents <- function(syneco, input_data,
                        g = NULL, 
                     color_list =  list(bds = "white",
                                          interior = "gray60",
                                          roads = "gray10",
                                          agents = "darkorchid3",
                                          envs = c("#E69F00", "#56B4E9",
                                                   "#009E73", "#F0E442", "#0072B2",
                                                   "#D55E00", "#CC79A7"))){

    if(is.null(g))     g <- ggplot2::ggplot() # Set up empty plot

    households <- do.call('rbind', lapply(syneco, "[[", 1))
    g <- g + ggplot2::geom_point(data = households,
                                 ggplot2::aes(longitude, latitude),
                                 alpha = 1, col = color_list$agents)
    coords_df <- data.frame(sp::coordinates(input_data$shapefile),
                            place_id = input_data$shapefile@data$place_id)
    g <- g + geom_text(data = coords_df,
                       ggplot2::aes(x= X1, y = X2, label = place_id),
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
     g <- g + ggplot2::geom_path( data = roads, ggplot2::aes(x=long, y = lat, group = group),
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

    if(is.null(g))     g <- ggplot2::ggplot() # Set up empty plot
    cols <- rep(color_list$envs, length.out = length(unique(input_data$environments$Type)))
    col_scale <- ggplot2::scale_colour_manual(name = "reg", values = cols)
    g <- g + ggplot2::geom_point(data = input_data$environments,
                        ggplot2::aes(x = longitude, y = latitude,
                                     col = Type, group = Type),  shape = 17, size = 5) + col_scale
    return(g)
}

#' Add the labels and the theme to the plot
#'
#' @param region_name string, will become the title of the plot
#' g a ggplot.  Default is NULL.
plot_labs <- function(region_name, g = NULL){
    if(is.null(g))     g <- ggplot2::ggplot() # Set up empty plot

    g <- g + ggtitle(paste(region_name, "Synthetic Ecosystem")) + 
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
