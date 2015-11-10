  # 2015-1-4 WASE V2 Config File Beka Steorts, Jerzy Wieczorek, Shannon
# Gallagher This script does the following: Takes a geographyFileName
# (inputFolder and shapeFileType) and returns the geographical shape of
# the file Creates a table of counts using (inputFolder, countryName, and
# whether or not the country has PUMS) reads in the microdata given
# inputFolder, PUMScountry and outputs the number of PUMs in terms of
# people

# INPUTS: inputFolder, geographyFileName, shapeFileType OUTPUTS
# appropriate shapefile FUNCTION: reads in the geo shapefile
readGeography <- function(inputFolder, geographyFileName, shapeFileType, 
                          countryName, isUSA = FALSE, dataFolder) {
  require(maptools)
  if (shapeFileType == "world") {
    path <- paste0(inputFolder, "shapefiles/", "world/")
  } else if (shapeFileType == "mapMakerUK") {
    path <- paste0(inputFolder, "shapefiles/", "mapmakerUK/")
  } else if (shapeFileType == "gadm") {
    path <- paste0(dataFolder, "shapefiles/", "gadm/")
  } else if (shapeFileType == "usa2010") {
    path <- dataFolder
  }
  # add in other if statementsn
  shape <- readShapeSpatial(paste0(path, geographyFileName))
  if (shapeFileType == "mapMakerUK") {
    shape@data$geoLevel0 <- countryName
  } else if (shapeFileType == "world") {
    shape <- shape[countryName == sapply(shape$CNTRY_NAME, stripWhitePunct), ]
  }
  return(shape)
}


# INPUTS: inputFolder, PUMScountry (surrogate PUMS country) OUTPUTS:
# persons PUMS datafile FUNCTION: reads in microdata
readMicroData <- function(inputFolder, PUMScountry, countryName, isUSA, PUMS.p, 
                          dataFolder) {
  if (isUSA) {
    path <- paste0(dataFolder, PUMS.p)
    pums.p <- fread(path)
    return(as.data.frame(pums.p))
  } else {
    path <- paste0(dataFolder, "PUMS/", PUMScountry)
    pums.p <- fread(path)
  }
  return(as.data.frame(pums.p))
}

# ATTN: Ask Bill if we should save scraped files
readCounts <- function(inputFolder, countryName, hasPUMS = TRUE, year) {
  if (hasPUMS) {
    countsTable <- getSampleCounts(inputFolder, countryName)
  } else {
    countsTable <- scrapeGeohive(countryName, year)
  }
  return(countsTable)
} 
