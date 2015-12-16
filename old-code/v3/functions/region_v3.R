# 2015-1-4 Region Beka Steorts, Jerzy Wieczorek, Shannon Gallagher One
# function that generates the households within a region And an apply
# function


# FUNCTIONS: Genates the household coordinates INPUTS: regionNames (char
# vec), regionCounts, formattedShapefile, numDecLatLong, hasPUMS,
# geoLevel OUTPUTS: 2 column data frame of latitude and longitude coords
# to numDecLatLong places Requirements: source sampleFromRegion
# subsetShapefile Notes: Assuming that the formattedShapefile has a
# GEO_ID for the region to match on and also assuming that all regions
# that are named similarly are unioned together


generateRegionCoords <- function(regionRow, regionNames, regionCounts, formattedShapefile, 
                                 numDecLatLong = 8, hasPUMS = TRUE, geoLevel = 1, geoSampling = "random") {
  # subset theshapefile sample regionCounts many coords TODO:
  # sampleFromRegion function make into data frame format appropriately
  # return coordindates)
  
  subsettedShapefile <- subsetShapefile(regionNames, formattedShapefile, 
                                        numDecLatLong, geoLevel)
  coords <- sampleFromRegion(regionRow, regionCounts, subsettedShapefile, 
                             numDecLatLong)
  return(coords)
}


# INPUT: regionNames, character vector (one per region name, i.e.
# ('Sierra Leone', 'Bo', Dist. 2'), formattedShapefile (has geoLevels and
# regions unioned appropriately), geoLevel (0 for country, 1 for level 1
# geog, 2 for level 2 geog, etc. OUTPUT: subsetted shapefile to region of
# interest FUNCTION: subsets shapefile to appropriate region (dependent
# on geolevel) REQUIREMENTS: NONE TODO: vectorize
subsetShapefile <- function(regionNames, formattedShapefile, numDecLatLong, 
                            geoLevel = 1) {
  subShape <- formattedShapefile
  for (level in 0:geoLevel) {
    curRegion <- tolower(regionNames[level + 1])  #since level begins at 0
    colRegs <- tolower(as.character(subShape@data[, paste0("geoLevel", 
                                                           level)]))
    subShape@data <- subset(subShape@data, colRegs == curRegion)
  }
  return(subShape)
}




# INPUT: regionCounts, subsettedShapefile, numDecLatLong OUTPUT:
# dataframe of longitude (x) and latitude (y) coordinates from the
# specified region FUNCTION: sample points from a shapefile
# REQUIREMENTS:sp

sampleFromRegion <- function(regionRow, regionCounts, formattedShapefile, 
                             numDecLatLong = 8) {
  s.slot <- slot(formattedShapefile, "polygons")
  if (regionCounts > 0) {
    require(sp)
    newRegionRow <- which(formattedShapefile@plotOrder == regionRow)
    s <- spsample(s.slot[[newRegionRow]], n = regionCounts, offset = c(0, 0), type = "random", iter = 50)
    
    # Temporary fix for puerto rico to sample from the nearest location :(
    if (is.null(s)) {
      warning("Wasn't able to sample from the correct region")
      s <- spsample(s.slot[[newRegionRow + 1]], n = regionCounts, offset = c(0, 
                                                                             0), type = "random", iter = 50)
    }
    npts <- nrow(s@coords)
    stopifnot(npts == regionCounts)  # if we have number of points, stop
    latLong <- s@coords
    df.out <- data.frame(Long = latLong[, 1], Lat = latLong[, 2])
    df.out <- round(df.out, numDecLatLong)
  } else {
    stop("0 Count!")
  }
  return(df.out)
}

