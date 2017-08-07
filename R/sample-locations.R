#' Generic sampling locations function 
#' 
#' @param method character vector either "uniform" or "roads" determining 
#' how we are sampling locations 
#' @param place_id numeric specifiying the ID of the region we are 
#' subsampling  
#' @param n_house numeric indicating the number of households
#' @param shapefile sp class with all of the locations for each place id. Note 
#' that this is a list with two shapefiles if me
#' @param noise the standard deviation of how much 
#' we jitter the road locations in each direction (only if method is "roads")
#' 
#' @return SpatialPoints object with coordinates for the n households
sample_locations <- function(method, place_id, n_house, shapefile, noise = .001, shapefile_id = NULL) {
  # Call the appropriate location sampling function
  if (method == "uniform") {
    locs <- sample_locations_uniform(place_id, n_house, shapefile, noise, shapefile_id)
  } else if (method == "roads") {
      locs <- sample_locations_roads(place_id, n_house, shapefile, noise, shapefile_id)
  } else {
    stop("location sampling method must be uniform or roads")
  }

  # If locs are NULL, return an error!
  if (is.null(locs)) {
    stop(paste0("Error place, ", place_id, ": sample locations returned NULL!"))
  }
  
  return(locs)
}

#' Sample from a particular polygon shapefile 
#' 
#' @param place_id numeric specifiying the ID of the region we are
#' subsampling
#' @param n_house numeric indicating the number of households
#' @param shapefile sp class with all of the locations for each place id
#' @param noise numeric indicating how must noise to add to sampled points
#' This is only used if the number of points to sample exceeds 100,000, in which 
#' case the spsample function takes too long. Instead, we sample 100,000 points, 
#' sample n_house from these 100,000, and then add random noise.
#' @return SpatialPoints object with coordinates for the n households
sample_locations_uniform <- function(place_id, n_house, shapefile, noise = .001, shapefile_id = NULL) {
  # Extract the index of the appropriate polygon 
  if (!is.null(shapefile_id)) {
    region <- which(shapefile$shapefile_id == shapefile_id)
  } else {
    region <- which(shapefile$place_id == place_id)
  }
  
  # If the shapefile also has roads, subset the 
  # shapefile version instead 
  if (class(shapefile) == "list") {
    shapefile_ind <- which(names(shapefile) == "shapefile")
    shapefile <- shapefile[[shapefile_ind]]
    region <- which(shapefile$place_id == place_id)
  }
  
  # Subset the shapefile to the polygon specified by the place_id argument 
  slots <- methods::slot(shapefile, "polygons")
  if(length(region) <= 0){
      print(shapefile_id)
      print(place_id)
      print(head(shapefile$place_id))
      stop()
  }
  poly <- slots[[region]]
  
  # Remove holes from polygon if any are found 
  is_hole <- lapply(poly@Polygons, function(p) p@hole)
  if (any(unlist(is_hole)) == TRUE) {
    poly <- remove_holes(poly)
  }
  
  # Obtain a Uniform, simple random sample of size n_house. If there 
  # are less the 100K points, use the 
  if (n_house < 100000) {
    locs <- sp::spsample(poly, n = n_house, offset = c(0, 0), 
                         type = "random", iter = 50)
  } else {
    # Sample 100,000 points, then sample n_house of these with replacement 
    locs <- sp::spsample(poly, n = 100000, offset = c(0, 0), 
                         type = "random", iter = 50)
    sample_inds <- sample(x = 1:100000, size = n_house, replace = TRUE)
    locs@coords <- locs@coords[sample_inds, ]
    
    # Add noise so we don't duplicate poitns 
    locs@coords[, 1] <- locs@coords[, 1] + rnorm(n_house, 0, noise)
    locs@coords[, 2] <- locs@coords[, 2] + rnorm(n_house, 0, noise)
  }
  
  return(locs)
}

#' Remove holes from an object of class Polygon 
#' 
#' @param polygon object of class Polygon or Polygons 
#' 
#' @note Borrowed the idea from the wild1 package, which 
#' I wasn't able to load for R 3.2.2, so I found the source code here:
#' https://github.com/cran/wild1/blob/master/R/remove.holes.r
#' 
#' @return polygon without and Polygons with holes 
remove_holes <- function(polygon) {
  is_hole <- lapply(polygon@Polygons, function(p) p@hole)
  is_hole <- unlist(is_hole)
  polys <- polygon@Polygons[!is_hole]
  polygon <- Polygons(polys, ID = polygon@ID)
  return(polygon)
}

#' Sample coordinates from roads
#'
#' @param place_id numeric specifiying the ID of the region we are 
#' subsampling  
#' @param n_house numeric indicating the number of households
#' @param shapefile sp class with all of the locations for each place id.  
#' In addition, we must have road shapefiles so shapefile is a list with both 
#' the tracts and the roads, tracts is the first object and roads the second.
#' @param noise the standard deviation of how much we jitter the road locations in each direction
#' 
sample_locations_roads <- function(place_id, n_house, shapefile, noise = .0001, shapefile_id) {
  stopifnot(any(names(shapefile) == "roads"))
  
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("rgeos needed for sample_locations_roads to work.", call. = FALSE)
  }
  
  # Get the intersection of the boundary shapefile  and the roads shapefile
  new_shp <- subset_shapes_roads(place_id, shapefile)
  
  # If the new shape is NULL, sample uniform instead of roads  
  if (is.null(new_shp)) {
    warning("Can't sample from roads, sampling uniformly")
    locs <- sample_locations_uniform(place_id, n_house, shapefile[[1]], shapefile_id)
    return(locs)
  }
  
  # Sample from the roads shapefile with noise added 
  locs <- samp_roads(n_house, new_shp, noise)
  return(locs)
}

#' Subset the shapefile and road lines to proper roads within specified tract
#'
#' @param place_id numeric specifiying the ID of the region we are 
#' subsampling  
#' @param shapefile sp class with all of the locations for each place id.  
#' In addition, we must have road shapefiles so shapefile is a list with both the 
#' tracts and the roads, tracts is the first object and roads the second.
#' 
#' @return new_shp - roads within the tract, a SpatialLines object
subset_shapes_roads <- function(place_id, shapefile) {
  stopifnot(class(shapefile) == "list")
  stopifnot(length(shapefile) == 2)
  stopifnot(class(shapefile[[1]]) == "SpatialPolygonsDataFrame")
  
  # Subset the regions to the place_id polygon
  regions <- shapefile[[1]]
  poly <- regions[regions@data$place_id == place_id, ]

  # If roads is a file-path, read the roads file directly. This is used 
  # on Olympus because loading all the roads at once is too memory intensive, 
  # so we replace the SpatialLinesDataFrame with a file-path
  if (class(shapefile$roads) == "character") { 
    # Extract the place-county ID. If it's not there, then 
    # return NULL, which triggers uniform sampling
    place_county <- substr(place_id, 1, 5)

    # Read in the specific roads shapefile. If there's no 
    # corresponding file for this county, return NULL 
    # which will call the uniform sampling instead 
    roads_sub <- read_roads(path_to_roads = shapefile$roads, road_id = place_county)
      
  } else{ # otherwise we use the roads provided
      roads_sub <- shapefile$roads
  }
  
  if (is.null(roads_sub)) {
    return(NULL)
  }
  
  # Subset the potential roads, intersect with the 
  # polygon, verify it's the right class than return the 
  # final intersected shapefile 
  potential_roads <- roads_sub[poly, ]
  new_shp <- rgeos::gIntersection(potential_roads, poly, drop_lower_td = TRUE)
  stopifnot(class(new_shp) == "SpatialLines" | class(new_shp) == "SpatialPoints")
  
  return(new_shp)
}

#' Sample the locations from a SpatialLines object
#'
#' @param n_house number of households
#' @param new_shp SpatialLines or Spatial Points object
#' @param noise std deviation of Gaussian noise added to coordinates, default is .001
#' @note When the class is Spatial Points, the following sampling 
#' method takes place: "When x is of a class deriving from Spatial-class for which 
#' no spsample-methods exists, sampling is done in the bounding box 
#' of the object, using spsample.Spatial"
#'s This was added because the Puerto Rico intersection gave SpatialPoints
#' @return SpatialPoints object with coordinates for the n_house households
samp_roads <- function(n_house, new_shp, noise) { 
  stopifnot("lineobj" %in% slotNames(new_shp) | "lines" %in% slotNames(new_shp) | 
              class(new_shp) == "SpatialPoints")
  
  # Sample from the lineobj of the intersected Spatial 
  # object or the SpatialLines object. A few checks are in here to 
  # make sure we still sample if there is no lineobj slot, and 
  # if there is too few points 
  pts <- NULL 
  orig_n_house <- n_house
  while (is.null(pts)) {
    if ("lineobj" %in% slotNames(new_shp)) {
      pts <- sp::spsample(new_shp@lineobj, n = n_house, type = "random", iter = 50)
    } else if ("lines" %in% slotNames(new_shp)) {
      pts <- sp::spsample(new_shp, n = n_house, type = "random", iter = 50)
    } else if (class(new_shp) == "SpatialPoints") {
      pts <- sp::spsample(new_shp, n = n_house, type = "random", iter = 50)
    } else {
      stop("Roads shapefile must have lines or lineobj as slotNames!")
    }
    
    # If the number of households is too low, add ten and 
    # try to re-sample. Eventually, we will have enough samples 
    # to get the required samples. This page:
    # https://github.com/edzer/sp/blob/master/R/spsample.R
    # shows the details, lines 178-181 show how the number of samples 
    # is determined. 
    if (is.null(pts)) {
      n_house <- n_house + 10
    }
  }
  
  # Sometimes sampling fails to get the exact number of households correct.  
  # If so, we resample from already selected points to fill in the rest. If 
  # we sample too many points in a similar way, we can subset the correct amount 
  num_points <- length(pts)
  if (num_points < n_house) {
    resampled_pts <- n_house - num_points
    inds <- sample(1:num_points, resampled_pts, replace = TRUE)
    pts <- pts[c(1:num_points, inds), ]
  } else if (num_points > n_house) {
    pts <- pts[1:n_house, ]
  }
  stopifnot(n_house == length(pts))
  
  err_x <- rnorm(length(pts), 0, noise)
  err_y <- rnorm(length(pts), 0, noise)
  pts@coords[, 1] <- pts@coords[, 1] + err_x
  pts@coords[, 2] <- pts@coords[, 2] + err_y
  
  # If we needed to increase the number of samples, 
  # make sure we re-set the number of households with locations
  if (n_house != orig_n_house) {
    pts <- pts[1:orig_n_house, ]
  }
  stopifnot(orig_n_house == length(pts))
  
  return(pts)
}
