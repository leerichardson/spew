# 2015-1-4 Region Beka Steorts, Jerzy Wieczorek, Shannon Gallagher
# Functions dealing with counts and counts tables to check if microdata
# is in standard form, otherwise, convert it.


# FUNCTION: load/scrape from web/ calculate from PUMS the counts for the
# given geolevels for a country INPUTS: countryName (string), year (4
# digit int), geoLevel (int), pums.p (data frame), params (list),
# formattedShapefile (standardized shapefile), inputFolder (string),
# version (string 'vX'), userParams (list) OUTPUTS: a N+2 column data
# frame where N is the geoLevel and one column for the counts in that
# region (uniquely identified by the previous N+1 cols) REQUIREMENTS:
# getCountsFromPUMS, loadCountsTable, scrapeGeohive,
# matchGeohiveWithShapefile
getRegionCountsDF <- function(countryName, year, geoLevel, stdPUMS, params, 
                              formattedShapefile, inputFolder, version, userParams = userParams, geohiveLevel, 
                              dataFolder, geoID) {
  # 3 cases for params$table ('PUMS', 'pretab', 'geohive')) if 'PUMS' then
  # calculate counts using pums. if 'pretab' load a pretabulated table if
  # 'geohive', scrape from geohive if the country doesn't have its own
  # PUMS, then divide the counts by the household average return a data
  # frame with uniquely identified regions and counts
  browser()
  switch(params$table, USPUMS = {
    countsDF <- read.csv(paste0(dataFolder, "popTables/households.csv"))
    countsDF <- countsDF[, 1:4]
    ids <- t(sapply(countsDF$Id2, breakUpUSID))
    colnames(ids) <- c("geoLevel0", "geoLevel1", "geoLevel2")
    countsDF <- cbind(countsDF, ids)
    countsDF <- countsDF[, c(5:7, 4)]
    countsDF$geoLevel0 <- as.character(countsDF$geoLevel0)
    countsDF$geoLevel1 <- as.character(countsDF$geoLevel1)
    countsDF$geoLevel2 <- as.character(countsDF$geoLevel2)
    colnames(countsDF)[4] <- "counts"
    
    # combine shapefile and counts
    shape.df <- formattedShapefile@data[, c(1:3, ncol(formattedShapefile@data))]
    countsDF <- merge(shape.df, countsDF, by = c("geoLevel0", "geoLevel1", 
                                                 "geoLevel2"))
    # ATTN: MUST FIX following problem!!!!!
    countsDF$counts[is.na(countsDF$counts)] <- floor(median(countsDF$counts, 
                                                            na.rm = T))
  }, PUMS = {
    countsDF <- getCountsFromPUMS(geoLevel, stdPUMS)
  }, pretab = {
    countsDF <- loadCountsTable(inputFolder, countryName, year, geoLevel, 
                                version)
    countsDF$geoLevel0 <- countryName
    countsDF <- subset(countsDF, select = -GEOLEV1)
    names(countsDF)[names(countsDF) == "REGION"] <- "geoLevel1"
    countsDF <- countsDF[, c("geoLevel0", "geoLevel1", "counts")]
  }, geohive = {
    countsDF <- scrapeGeohive(dataFolder, countryName, year, geolevel, 
                              geohiveLevel)
    countsDF <- stdizeCounts(countsDF, geoLevel, params)
    colsToKeep <- (which(!(is.na(params$countsGeoNames))) - 1)
    countsDF$counts <- as.integer(gsub("[[:punct:]]", "", countsDF$counts))
    countsDF <- matchGeohiveWithShapefile(countsDF, formattedShapefile, 
                                          geoLevel, params, userParams)
    countsDF <- countsDF[sort(colnames(countsDF))]
  })
  
  # divide by the household average to get approximate people count
  if (params$hasPUMS == FALSE) {
    countsDF$counts <- ceiling(countsDF$counts/params$hhAve)
  }
  
  return(countsDF)
}


# Function: get the household counts per region from the PUMS INPUTS:
# geoLevel (int), stdPUMS (data frame) OUTPUTS: N+2 column data frame
# first N+! columns geoLevels, last col counts REQUIREMENTS:

getCountsFromPUMS <- function(geoLevel, stdPUMS) {
  # find unique regions IDs in geoLevelX aggregate the counts within that
  # region return data frame with N+2 columns first N+1 columns as the
  # geoLevel and last column as the household counts
  stopifnot("WTHH" %in% colnames(stdPUMS))
  weights <- stdPUMS$WTHH
  
  # aggregating the weights using the sum of them by the geo region
  agg <- aggregate(weights, by = list(GEOLEV1 = stdPUMS$uniID), sum)  # grab the weights from PUMS
  geoLevelList <- strsplit(agg$GEOLEV1, " ")
  geoMat <- do.call(rbind, geoLevelList)
  countsDF <- data.frame(geoMat, stringsAsFactors = FALSE)
  colnames(countsDF) <- paste0("geoLevel", 0:geoLevel)
  countsDF$counts <- agg$x
  stopifnot(ncol(countsDF) == (geoLevel + 2))
  
  # return df or table of region and aggregate weights
  return(countsDF)
}

# FUNCTION: load a pretabulated counts table INPUTS: inputFolder
# (string),countryName (string), year (4 digit int), geoLevel (int),
# version (string) OUTPUT: N+1 (or 2 depending on whether geoLevel0 is
# included) col data frame with region ID and counts REQUIREMENTS: NONE
loadCountsTable <- function(inputFolder, countryName, year, geoLevel, version = "v2") {
  tabID <- paste0(year, countryName, geoLevel, version)
  tablePath <- paste0(inputFolder, "popTables/", tabID, "_popTable.csv")
  countsDF <- read.csv(tablePath)
  return(countsDF)
}


# FUNCTION: scrape geohive for the region counts INPUTS: countryName
# (string), year (4 digit int), geoLevel (int) OUTPUTS: N+2 col data
# frame with geoLevel0, ..GeoLevelN, and counts REQUIREMENTS: TODO:
# Create this function
scrapeGeohive <- function(dataFolder, countryName, year, geoLevel, geohiveLevel) {
  # stop('The scrapeGeohive function is under construction!') grab the
  # expanded file if it exists. If not, grab the admin file
  if (file.exists(paste0(dataFolder, "counts/", countryName, "_extended.csv"))) {
    path <- paste0(dataFolder, "counts/", countryName, "_extended.csv")
  } else if (file.exists(paste0(dataFolder, "counts/", countryName, "_admin.csv"))) {
    path <- paste0(dataFolder, "counts/", countryName, "_admin.csv")
  }
  
  # subset the geohive counts based on the global 'level' variable.
  countsDF <- read.csv(path, stringsAsFactors = FALSE)
  if (geohiveLevel != "NULL") {
    countsDF <- countsDF[countsDF$level == geohiveLevel, ]
  }
  
  # extract the proper year for the counts, rename it
  namesDF <- colnames(countsDF)
  popYear <- which(grepl(as.character(year), namesDF))
  colnames(countsDF)[popYear] <- "counts"
  
  # remove all the counts with '0' population... and also the ones which
  # have the same prefix in all locations...
  countsDF <- countsDF[countsDF$counts > 0, ]
  for (i in 1:2) {
    countsDF[, i] <- gsub("District of ", "", countsDF[, i])
    countsDF[, i] <- gsub("Sector de ", "", countsDF[, i])
    countsDF[, i] <- gsub("Region de ", "", countsDF[, i])
    countsDF[, i] <- gsub("Cercle de ", "", countsDF[, i])
    countsDF[, i] <- gsub("Departement de ", "", countsDF[, i])
  }
  
  # removing whitespace from column names then return the dataframe
  colnames(countsDF) <- gsub("[^[:alnum:]]", "", colnames(countsDF), fixed = FALSE)
  return(countsDF)
}

# FUNCTION: match region names and merge from countsDF to shapefiles
# INPUTS: countsDF (data frame), formattedShapefile (shapefile), geoLevel
# (int) OUTPUTS: modified countsDF with region names that match the
# formattedShapefile REQUIREMENTS: findMatchingNames, findMatchingInds,
# writeMatches, plyr TODO: Expand and improve this function
matchGeohiveWithShapefile <- function(countsDF, formattedShapefile, geoLevel, 
                                      params = params, userParams = userParams) {
  require(plyr)
  browser()
  matchingNames <- findMatchingNames(countsDF, formattedShapefile@data)
  matchingInds <- findMatchingInds(countsDF, formattedShapefile@data, matchingNames, 
                                   linkage = "jw")
  subsettedCounts <- countsDF[matchingInds, ]
  # writeMatches(formattedShapefile@data, subsettedCounts, matchingNames,
  # userParams)
  outCountsDF <- cbind(formattedShapefile@data[paste0("geoLevel", 0:2)], 
                       subsettedCounts["counts"])
  
  return(outCountsDF)
}

# FUNCTION: Be able to see which shapefiles were matched to which geohive
# numbers INPUT: df (shapefile df), subsettedCounts (df same size as df
# of the geohive counts), matchingNames (char vec), params (
# REQUIREMENTS: none
writeMatches <- function(df, subsettedCounts, matchingNames, userParams) {
  dfMatches <- cbind(df[matchingNames], subsettedCounts[matchingNames])
  colnames(dfMatches) <- c("shapefile_geolevel2", "geohive_geolevel2")
  path <- paste0(userParams$outputFolder, "shapefileAndGeohiveMatches.csv")
  write.csv(dfMatches, path, row.names = FALSE)
  return(TRUE)
}

# FUNCTION: break up 11 digit code into state/county/tract INPUTS:
# geoID2( character/integer thing) OUTPUTS: length 3 character vector)
# REQUIREMENTS: TODO:
breakUpUSID <- function(geoID2) {
  geoID2 <- ifelse(nchar(geoID2) == 10, paste0(0, geoID2), geoID2)  #if the census is dumb, add a zero
  geoid <- as.character(geoID2)
  st <- substr(geoid, 1, 2)
  co <- substr(geoid, 3, 5)
  tract <- substr(geoid, 6, nchar(geoid))
  return(c(st, co, tract))
}



# PURPOSE: Find the geoLevels to match one INPUTS: countsDF (data frame),
# df (shapefile data frame) OUTPUTS: matchedNames (charVec) REQUIREMENTS:
findMatchingNames <- function(countsDF, df) {
  browser()
  namesCounts <- colnames(countsDF)
  namesShape <- colnames(df)
  matchedNames <- namesCounts[namesCounts %in% namesShape]
  return(sort(matchedNames))
}

# PURPOSE: find the best matching inds to the shapefile names in the
# countsDf INPUTS: countsDF (data frame), df (shapefile dataframe),
# matchingNames (character vector), linkage (character) OUTPUTS:
# matchingInds (integer vector) REQUIREMENTS: RecordLinkage, linkMatch
findMatchingInds <- function(countsDF, df, matchingNames, linkage = "jw") {
  countsDFsub <- countsDF[matchingNames]
  dfSub <- df[matchingNames]
  if (linkage == "jw") {
    require(RecordLinkage)
    matchingInds <- linkMatch(dfSub, countsDFsub, linkFxn = jarowinkler)
    return(matchingInds)
  } else {
    stop("Enter a proper linkage!  You are in the findMatchingInds function")
    return(FALSE)
  }
}

# PURPOSE: find the best matching pair INPUTS: dfSub (dataFrame of
# geoLevels), countsDFsub(data frame of geolevels), linkFxn
# (recordLinkage fxn) OUTPUTS: matchingInds (integer vector)
# REQUIREMENTS: matchRLVec
linkMatch <- function(dfSub, countsDFsub, linkFxn, numMatchOn = "one") {
  if (numMatchOn == "one") {
    n <- ncol(dfSub)
    shapefileGeo <- as.character(dfSub[, n])
    countsGeo <- as.character(countsDFsub[, n])
    matchedInds <- sapply(shapefileGeo, matchRLVec, countsGeo, linkFxn)
    return(matchedInds)
  } else if (numMatchOn == "all") {
    stop("Matching on all geoLevels is under construction!")
    return(FALSE)
  }
}

# FUNCTION: does the link function on a single shapefileGeo to all of the
# countsGeo returns the ind of the max match INPUTS: shapefileGeo
# (character), countsGeo (character vector), linkFxn (record linkage
# function) OUTPUTS: matchedInd (integer) REQUIREMENTS: RecordLinkage
matchRLVec <- function(shapefileGeo, countsGeo, linkFxn) {
  linkScores <- linkFxn(shapefileGeo, countsGeo)
  matchedInd <- which.max(linkScores)
  return(matchedInd)
}

# FUNCTIONS: Conversion to standard form of counts INPUTS: inputFolder,
# countryName, hasPUMS=TRUE, year, regionName, regionCounts,
# formattedShapefile, numDecLatLong, params OUTPUTS: either exits or
# converts counts into standard format REQUIREMENTS: TODO: FINISH THIS
# FUNCTION


convertCounts <- function(inputFolder, countryName, hasPUMS = TRUE, regionName, 
                          regionCounts, formattedShapefile, numDecLatLong, params) {
  if (instdFormat(inputFolder, countryName, hasPUMS = TRUE) == FALSE) {
    # then put the file in standard format match with the geography from the
    # region now put in order return the converted counts
  }
  return(TRUE)
}


inStdFormatCounts <- function(inputFolder, countryName, hasPUMS = TRUE, year) {
  # write a function to see if the counts are in standard format
  return(TRUE)
}


