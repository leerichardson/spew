# 2015-1-4 Region Beka Steorts, Jerzy Wieczorek, Shannon Gallagher
# Function to check if shapefiles and micro data are in standard form,
# otherwise, convert it.

# 2015-01-08 SKG ATTN: the below functions need tested ATTN: need to
# write sorting function for PUMS

# FUNCTION: adds the PUMA for a given tract INPUTS: df (data frame);
# dataFolder (string OUTPUTS: df with a PUMA column REQUIREMENTS: plyr
# TODO: make faster
addPUMA <- function(df, dataFolder) {
  # Initialize the PUMA column to be added and read in the lookup table to
  # be used
  PUMA <- numeric(nrow(df))
  lookupTable <- read.csv(paste0(dataFolder, "tables/lookup10.csv"), header = TRUE)
  
  # Loop through each row in the data frame and add in the corresponding
  # puma based on the state and county
  for (row in 1:nrow(df)) {
    
    # Grab the desired state and county from which we want to obtain the
    # corresponding PUMA
    st1 <- as.integer(as.character(df$STATEFP10[row]))
    co1 <- as.integer(as.character(df$COUNTYFP10[row]))
    sample.inds <- which(lookupTable$STATEFP == st1 & lookupTable$COUNTYFP == 
                           co1)
    if (length(sample.inds) == 1) {
      ## SAM CHANGED THIS TO 1 FROM 2 ON 7/24/15
      puma.ind <- sample.inds
    } else {
      puma.ind <- sample(sample.inds, 1)
    }
    PUMA[row] <- lookupTable$PUMA5CE[puma.ind]
  }
  return(PUMA)
}

# FUNCTIONS: Conversion to standard form of geography (shapefile) INPUTS:
# inputFolder, countryName, hasPUMS=TRUE, year, formattedShapefile,
# numDecLatLong, geoLevel, year, params OUTPUTS: either exits or converts
# counts into standard format REQUIREMENTS: inStdFormatGeo,
# loadShapefileParams

stdizeShapefile <- function(inputFolder, countryName, hasPUMS = TRUE, formattedShapefile, 
                            numDecLatLong, geoLevel, year, params, isUSA, dataFolder) {
  if (isUSA) {
    formattedShapefile@data$PUMA <- addPUMA(formattedShapefile@data, 
                                            dataFolder)
    colnames(formattedShapefile@data)[1:3] <- c("geoLevel0", "geoLevel1", 
                                                "geoLevel2")
    formattedShapefile@data$geoLevel0 <- as.character(formattedShapefile@data$geoLevel0)
    formattedShapefile@data$geoLevel1 <- as.character(formattedShapefile@data$geoLevel1)
    formattedShapefile@data$geoLevel2 <- as.character(formattedShapefile@data$geoLevel2)
    return(formattedShapefile)
  } else {
    if (inStdFormatGeo(formattedShapefile, countryName, isPUMS = FALSE, 
                       hasPUMS, geoLevel) == FALSE) {
      
      # load a character vector of shapefile params what the actual names of
      # geoLevel0, ..geoLeveln, are
      countryShapefileParams <- params$shapefile
      formattedShapefile@data <- makeGeoLevelNames(countryShapefileParams, 
                                                   formattedShapefile@data)
    }
    formattedShapefile@data <- formattedShapefile@data[, order(names(formattedShapefile))]
    formattedShapefile@data$geoLevel0 <- countryName
    return(formattedShapefile)
  }
}


# INPUTS: inputFolder (char), countryName (char), hasPUMS (logical),
# regionNames (char vec), formattedShapefile (shapefile), numDecLatLong
# (int), geoLevel (int >=0), year (4 digit number), params OUTPUTS: a
# standardized PUMS with standardized geolevel names FUNCTION:
# standardize PUMS to standard geo level names REQUIREMENTS:
# inStdFormatGeo, makeGeoLevelNames

stdizePUMS <- function(inputFolder, pums.p, countryName, hasPUMS = TRUE, 
                       geoLevel, year, params) {
  if (inStdFormatGeo(pums.p, countryName, isPUMS = TRUE, hasPUMS, geoLevel) == 
      FALSE) {
    countryPUMSParams <- params$PUMS
    pums.p <- makeGeoLevelNames(countryPUMSParams, pums.p)
  }
  pums.p <- pums.p[, order(names(pums.p))]
  return(pums.p)
}

# FUNCTION: change names of objects to standardized geoLevel names
# INPUTS: parameters for dataObj (char vec) and the data obj (shapefile
# or df) OUTPUTS: dataObj with the proper geoLevel names REQUIREMENTS:
# NONE
makeGeoLevelNames <- function(params, dataObj) {
  objNames <- names(dataObj)
  for (i in 1:length(params)) {
    # rename the geoLevel
    ind <- which(objNames == params[i])
    names(dataObj)[ind] <- paste0("geoLevel", i - 1)
  }
  return(dataObj)
}


### by Abby trying to fix Jamaica
makeGeoLevelNamesCounts <- function(params, countsDF) {
  objNames <- names(countsDF)
  for (i in 1:length(params)) {
    ind <- which(objNames == params[i])
    names(countsDF)[ind] <- paste0("geoLevel", 1)
  }
  return(countsDF)
  
}

# INPUTS: dataObj (either a shapefile or PUMS dataframe), countryName
# (char), isPUMS (logical), hasPUMS(logical) OUTPUTS: logical FUNCTION:
# checks that the geography shapefile or PUMS are in standard format to
# run further programs REQUIREMENTS: NONE NOTES: isPUMS refers to the
# object being a PUMS file, hasPUMS refers to having IPUMS PUMS ATTN:
# This function needs to have better checks

inStdFormatGeo <- function(dataObj, countryName, isPUMS, hasPUMS, geoLevel) {
  if (isPUMS & !hasPUMS) {
    # if the obj is PUMS and does not have its own set of PUMS, nothing to be
    # done
    return(FALSE)
  }
  # if the countryName is not correct and the country has its own PUMS,
  # return FALSE
  objNames <- names(dataObj)
  levels <- 1:geoLevel
  levelNames <- paste0("geoLevel", levels)
  # next check that all the 'geoLevels' are in the object with the proper
  # name
  if (!(levelNames %in% objNames)) {
    return(FALSE)
  }
  if (countryName != unique(dataObj$geoLevel0) & hasPUMS == TRUE) {
    return(FALSE)
  }
  return(TRUE)
}

# FUNCTION: sort PUMS by geoLevel(s) INPUT: the standardized PUMS (df),
# geoLevel (int >=0), hasPUMS (logical) OUTPUT: sorted PUMS REQUIREMENTS:
# plyr

sortPUMS <- function(stdPUMS, geoLevel, hasPUMS) {
  # check if the country has its own PUMS if not, return regular PUMS since
  # we dont need them sorted if so, sort by country, geolevel 1, geolevel
  # 2, etc...
  stdPUMS <- stdPUMS[sort(colnames(stdPUMS))]
  newPUMS <- stdPUMS
  if (hasPUMS == TRUE) {
    require(plyr)
    newPUMS <- arrange(newPUMS, uniID)
  }
  return(newPUMS)
}

# FUNCTION: join shapefile with same unique GEOID together INPUT:
# stdShapefile OUTPUT: shapefileOut (a possibly condensed shapefile)
# REQUIREMENTS: pasteGeoLevels (miscFunctions.R) TODO: clean up this
# function

unionRegions <- function(stdShapefile, geoLevel) {
  df.sub <- stdShapefile@data[, paste0("geoLevel", 0:geoLevel)]
  df.sub <- sapply(df.sub, stripWhitePunct)
  geoids <- pasteGeoLevels(df.sub, geoLevel)
  
  # find which regions are duplicates
  geo.df <- data.frame(table(geoids))
  nRegs <- nrow(geo.df)
  # amend spatial data frame for each iterations should be vectorized for
  # v2
  shapefileOut <- stdShapefile
  shapefileOut@data$GEO_ID <- geoids
  
  for (ind in 1:nRegs) {
    if (geo.df$Freq[ind] > 1) {
      # for each unique region...
      print("in if")
      n <- length(shapefileOut)
      regInds <- which(geoids == geo.df$geoids[ind])  #which indices are duplicated
      regName <- as.vector(geo.df$geoids[ind])  #name of that region
      geoids <- c(geoids[-regInds], regName)  #making a new region #taking those regions out, adding it as one to end
      na.vec <- 1:length(shapefileOut)
      na.vec <- ifelse(na.vec %in% regInds, 1, NA)
      # the joined region
      joined.regions <- unionSpatialPolygons(shapefileOut, na.vec)
      joined.regions <- spChFIDs(joined.regions, as.character(geoids[regInds[1]]))
      # taking out the duped regions
      shapefileOut <- shapefileOut[-regInds, ]
      shapefileOut <- spChFIDs(shapefileOut, paste(ind, as.character((1:n)[-regInds])))
      # binding them together
      shapefileOut <- spRbind(shapefileOut, joined.regions)
      cur.names <- row.names(shapefileOut)
      df.temp <- data.frame(GEO_ID = geoids)
      rownames(df.temp) <- cur.names
      # add the attributes back in
      shapefileOut <- SpatialPolygonsDataFrame(shapefileOut, df.temp)
    }
  }
  
  shapefileOut@data$uniID <- as.character(shapefileOut@data$GEO_ID)
  shapefileOut@data[, paste0("geoLevel", 0:geoLevel)] <- as.character(shapefileOut@data[, 
                                                                                        paste0("geoLevel", 0:geoLevel)])
  # splitting the GEO_ID up again
  for (i in 1:nrow(shapefileOut@data)) {
    for (j in 0:geoLevel) {
      regName <- strsplit(shapefileOut@data$uniID[i], " ")[[1]]
      shapefileOut@data[i, paste0("geoLevel", j)] <- as.character(regName[j + 
                                                                            1])
    }
  }
  
  shapefileOut@data <- shapefileOut@data[, order(names(shapefileOut@data))]
  return(shapefileOut)
}

# PURPOSE: standardize and sort the counts population tables INPUTS:
# countsDF (data frame), geoLevel (int), params (character vector)
# OUTPUTS: countsDF (data frame)


stdizeCounts <- function(countsDF, geoLevel, params) {
  countsDF <- makeGeoLevelNames(params$countsGeoNames, countsDF)
  countsdF <- sortPUMS(countsDF, geoLevel, hasPUMS = FALSE)
  return(countsDF)
  
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



