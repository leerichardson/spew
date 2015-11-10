# Miscellaneous Functions 2015-1-11 Beka Steorts, Shannon Gallagher, Lee
# Richardson General/miscellaneous functions used in the rest of the
# program

# FUNCTION: paste the geoLevels of stdPUMS together INPUTS: stdPUMS (data
# frame), geoLevel (int) OUTPUTS: character vector REQUIREMENTS: NONE
# NOTES: Assumes stdPUMs has geoLevels next to each other in ascending
# order
pasteGeoLevels <- function(stdPUMS, geoLevel, collapse = " ") {
  ind0 <- which(colnames(stdPUMS) == "geoLevel0")
  uniID <- stdPUMS[, ind0]
  return(uniID)
}

# FUNCTION: check the joined data frame for duplicate entries, if there
# are simply divide the population by the number there are INPUT:
# df.joined (data frame 2 cols- GEO_ID; counts OUTPUT: df.out, the fixed
# data frame
checkForDups <- function(df.joined) {
  df.tab <- data.frame(table(df.joined$GEO_ID))
  colnames(df.tab) <- c("GEO_ID", "Freq")
  df.joined <- join(df.joined, df.tab, by = "GEO_ID")
  df.joined$counts <- floor(df.joined$counts/df.joined$Freq)
  df.out <- df.joined[, 1:2]
  return(df.out)
}

# PURPOSE: formats objName by ridding of white space and apostrophes,
# puts in lower case INPUT: objName OUTPUT: formatted formattedName
stripWhitePunct <- function(objName) {
  formattedName <- tolower(objName)
  formattedName <- gsub("\\s", "", formattedName)
  formattedName <- gsub("'", "", formattedName)
  return(formattedName)
}


# INPUTS: countryName (char), geoLevel (int), year (4 digit int),
# inputFolder(char), paramType (string - 'PUMS', 'shapefile', table,
# etc.) OUTPUTS: params (a list) FUNCTION: load the parameters (may move
# to config file)

loadParams <- function(countryName, geoLevel, year, version = "v2", inputFolder, 
                       paramType) {
  # TODO: write the loading of parameters function every
  # country/geolevel/year/type has a different name for each geographic
  # region we call a control file that refers to the country with its
  # geoLevel, year, and paramType (PUMS or shapefile) and return a
  # character vector of what is equivalent to c(geoLevel0Name,
  # geoLevel1Name, etc.) TODO: write an outline of the tree and control
  # file (.txt) will make a textfile with yearcountryNamegeoLevelvX format
  # i.e.  eg.  2008SierraLeone0v1 shapefile, 'CNTRY_NAME' 'GEOLEV1' PUMS,
  # 'CNTRY_NAME' 'GEOLEV1' table, 'geohive' 'pretab' 'PUMS' varsToKeepP
  # 'something' 'somethingelse' varsToKeepH 'horses' 'ponies' 'dogs'
}

# We take in the world shape files from
# #https://international.ipums.org/international/gis_harmonized_1st.shtml
# and return subfile that contains the user's country INPUT- country
# name, the world shapefile OUTPUT- shapefile of that country subsets the
# IPUMS world shapefile
subset_world <- function(country.name, world.shapefile) {
  country.name <- tolower(country.name)
  subshape <- subset(world.shapefile, tolower(world.shapefile$CNTRY_NAME) == 
                       country.name)
  return(subshape)
} 
