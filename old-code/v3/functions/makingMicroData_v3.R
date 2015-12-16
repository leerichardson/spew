# Making microData
# 2015-1-11
# Beka Steorts, Shannon Gallagher, Lee Richardson
# Functions pertaining making microdata, including
# Sampling from PUMS
# Sampling from Regions
# Assembling the household micro data
# Assembling the people micro data
# Writing out the data

#FUNCTION:  make and write microdata
#INPUTS:  params (list), stdPUMS.p (data frame), stdPUMS.h (data frame), stdShapefile (Shapefile), popTable (df), samplingScheme (char), inputFolder (char), outputFolder (char), geoLevel (int), countryName (char), year (4 digit int, version (char "vX"), hasPUMS (logical) 
#OUTPUTS:  logical vector
#REQUIREMENTS: outputMicroDataForRegion

outputMicroData <- function(params, stdPUMS.p, stdPUMS.h, stdShapefile, popTable, 
                           samplingScheme="unif", inputFolder, outputFolder, geoLevel, 
                           countryName, hasPUMS, year, version, codeFolder="functions/", 
                           numDecLatLong = 8, isUSA, extraVarsData, parallel = TRUE, 
                           dataFolder) {
  
  # don't run in parallel on my home computer, 
  # only when we're on Olympus. 
  if (Sys.getenv("USER") == "lee") {
    parallel <-  FALSE
  }
  
  # Either run the program in Parallel or 
  # one by one. Only using the one by one version 
  # right now for debugging purposes 
  if (parallel == TRUE) {
    library(doMC)
    library(foreach)
    nRegions<- nrow(popTable)
    ncores <- 64
    registerDoMC(ncores)
    microDataComplete <- foreach(index = 1:nRegions) %dopar% {
    print(paste0("Region ", index, " of ", nRegions))    
    outputMicroDataForRegion(index, popTable, params, stdPUMS.p, stdPUMS.h, stdShapefile, 
                               samplingScheme, inputFolder, outputFolder, geoLevel, countryName, 
                               hasPUMS, year, version, codeFolder, numDecLatLong, isUSA, extraVarsData,
                               dataFolder)
    }
    return(microDataComplete)    
  } else {
    # Loop through each region one by one and 
    # generate a synthetic population 
    nRegions<- nrow(popTable)
    for (index in 1:nRegions) {
      print(paste0("Region ", index, " of ", nRegions))
      microDataComplete <- outputMicroDataForRegion(index, popTable, params, stdPUMS.p, 
                                                    stdPUMS.h, stdShapefile, samplingScheme, inputFolder, 
                                                    outputFolder, geoLevel, countryName, hasPUMS, year,version, 
                                                    codeFolder, numDecLatLong, isUSA, extraVarsData, 
                                                    dataFolder)
      }
      return(microDataComplete)    
    }
}

#FUNCTION<- inner function for making and writing micro data for one region
#INPUTS:  regionRow (int), popTable (df), params (list), stdPUMS.p (data frame), stdPUMs.h (df), stdPUMS.h (df), stdShapefile (shapefile), samplingScheme (char), inputFolder (char), outputFolder (char), geoLevel (int), countryName (char), hasPUMS (logical)
#OUTPUTS: logical
#REQUIREMENTS: pasteGeoLevels (miscFunctions), subsetPUMS, sampleFromPUMS, sampleFromGeog, 
#assembleSynthPUMS.h, assembleSynthPUMS.p, makeFileName, writePUMS
outputMicroDataForRegion<- function(regionRow, popTable, params, stdPUMS.p, 
                                    stdPUMS.h, stdShapefile, samplingScheme, inputFolder, 
                                    outputFolder, geoLevel, countryName, hasPUMS, year, 
                                    version, codeFolder, numDecLatLong, isUSA, extraVarsData, 
                                    dataFolder) {
  #subset PUMS.p and PUMS.h to the region in question (using hasPUMS)
  #get the proper region and counts from popTable
  #sample appropriately to get indices of stdPUMS.h
  #sample from the regions
  #assemble pums.h
  #ATTN:  2/15/2015 making a change in below line from commented out one to new one 
  #(UPDATE:  commented line is currently the change)
  uniIDPopTableRow<- popTable[regionRow,1:(geoLevel+1)] 
  if (isUSA) {
    regionID<- popTable$PUMA[regionRow]
    ID1 <- popTable$geoLevel1[regionRow]
    ID2 <- popTable$geoLevel2[regionRow]
  } else {
    regionID<-(uniIDPopTableRow[2])[1:3]
    ID1 <- popTable$geoLevel1[regionRow]
    ID2 <- popTable$geoLevel2[regionRow]
  }
  count<- popTable$counts[regionRow]
  if (isUSA & as.numeric(popTable$geoLevel0) == 6) {
    count <- ratio_adjust(count, ID1, dataFolder)
  }  
  if (count == 0) {
    return("This Region has a Population Count of 0!")
  }
  subsettedPUMSInds.h<- subsetPUMS(stdPUMS.h, regionID, type="h", hasPUMS, isUSA)
  synthInds.h<- sampleFromPUMS(count, subsettedPUMSInds.h, samplingScheme, 
                               params$hhAve, stdPUMS.h)
  coords<- sampleFromGeog(count, regionRow, popTable,  stdShapefile, 
                          geoLevel, inputFolder, codeFolder, numDecLatLong, params)
  synthPUMS.h<- assembleSynthPUMS.h(stdPUMS.h, synthInds.h, coords, params, 
                                    regionRow, popTable, geoLevel)
  synthPUMS.h<- makeSerialSynth(synthPUMS.h, isUSA)
  synthPUMS.p<- assembleSynthPUMS.p(stdPUMS.p, synthPUMS.h, params, geoLevel, 
                                    isUSA, extraVarsData, inputFolder)
  fileName<- makeFileName(ID1, ID2, samplingScheme, geoLevel, countryName, 
                          year, version)
  didWritePUMS.h<- writePUMS(ID1, ID2, synthPUMS.h, fileName, outputFolder, type="h")
  didWritePUMS.p<- writePUMS(ID1, ID2, synthPUMS.p, fileName, outputFolder, type="p")
  didWritePUMS<- didWritePUMS.h & didWritePUMS.p
  return(didWritePUMS)
}

#FUNCTION:  Adjust the tract counts based on travis' table. 
#OUTPUT indsOut 
#REQUIREMENTS:  NONE
ratio_adjust <- function(count, county_id, dataFolder) {
  ratios <- read.csv(paste0(dataFolder, "other/county_ratios.csv"))
  ratio_row <- which(ratios$county_id == as.numeric(county_id))
  county_ratio <- ratios[ratio_row, "ratio"]
  final_count <- trunc(county_ratio * count)
  return(final_count)
}


#FUNCTION:  subset pums appropriately by returning indices
#INPUTS:   stdPUMS.h (df), regionID (char), type (char), hasPUMS (logical)
#OUTPUT indsOut 
#REQUIREMENTS:  NONE
subsetPUMS <- function(stdPUMS.h, regionID, type="h", hasPUMS, isUSA) {
  if (hasPUMS == FALSE) {
    return(1:nrow(stdPUMS.h))
  }
  if (isUSA) {
    indsOut <- which(stdPUMS.h$PUMA == regionID)
    if (length(indsOut) < 1) {
      indsOut <- 1:nrow(stdPUMS.h)
    }
    
    # Special case for puerto rico. Only subset 
    # the data from the San Juan PUMA's for use 
    # in the dengue context
    if (stdPUMS.h$geoLevel0 == 72) {
      indsOut <- which(stdPUMS.h$PUMA %in% c(1001, 1002, 1003, 1004))
    }
    return(indsOut)
  }
  indsOut <- which(stdPUMS.h$uniID == regionID)
  return(indsOut)
}

#FUNCTION:  Sample from the PUMS indices based on the watned sample scheme
#INPUTS:  count (int), subsettedPUMSInds.h (int vector), samplingSc?heme (char), hhAve (real number), stdPUMS.h (data frame)
#OUTPUTS: vector of indices
#REQUIREMENTS: weightedSamplingKKT

sampleFromPUMS<- function(count, subsettedPUMSInds.h, samplingScheme, hhAve, stdPUMS.h){
  # if samplingScheme = "unif", sample directly from the indices
  # otherwise, solve the KKT conditions and do weighted sampling 
  switch(samplingScheme,

         unif = { 
           inds <- sample(subsettedPUMSInds.h, count, replace = TRUE)
         },
         weightedAve = { 
           inds <- weightedSamplingKKT(count, subsettedPUMSInds.h, hhAve, stdPUMS.h)
         }
  )
  return(inds)
}

#FUNCTION:  get the coordinates for the households within the region
#INPUTS:  count (int), regionRow (int), popTable (df), stdShapefile (shapefile)
#OUTPUTS:  2 column data frame with longitude and latitude coordinates of length count
#REQUIREMENTS:  source("region_v2.R")- generateRegionCoords 
#TODO:  Fill in this function

sampleFromGeog<- function(count, regionRow, popTable, stdShapefile, geoLevel,  
                          inputFolder, codeFolder, numDecLatLong, params=params){
  #put in proper functions from the region/geog files
  #ATTN:  get rid of v3 hardcoding 4/20/15
  path<- paste0(inputFolder, codeFolder, "region_v3.R")
  source(path) #source the appropriate functions
  #ATTN:  MADE A CHANGE TO PROGRAM 2/15/15 below
  if (!(params$table %in% c("geohive", "USPUMS"))){
    regionNames<- popTable[regionRow, 1:geoLevel]
  }
  else if (params$table =="geohive"){
    regionNames<- popTable[regionRow, 2:(geoLevel+2)]
  }
  else if(params$table=="USPUMS"){
    regionNames<- popTable[regionRow, 1:(geoLevel+1)]
  }
  coords<- generateRegionCoords(regionRow, regionNames, count, stdShapefile, numDecLatLong, geoLevel=geoLevel)
  return(coords)
}

#FUNCTION:  assemble the actual household synthetic data into a dataframe
#INPUTS:  stdPUMS.h (data frame), synthInds.h (int vector), coords (2 col data frame with Long and Lat), params (list), regionRow (int), popTable (df), hasPUMS (logical)
#OUTPUTS:  data frame of the synthetic households
#REQUIREMENTS:  NONE

assembleSynthPUMS.h<- function(stdPUMS.h, synthInds.h, coords, params, regionRow, popTable, geoLevel) {
  varsToKeep.h <- params$varsToKeepH
  synthPUMS.h<- stdPUMS.h[as.integer(synthInds.h),]
  synthPUMS.h<- subset(synthPUMS.h, select=c(varsToKeep.h))
  nObs<-  nrow(synthPUMS.h)
  curGeoLevels<- popTable[regionRow,2:3]
  geoLevelDF<- curGeoLevels[rep(1, nObs),]
  synthPUMS.h<- cbind(geoLevelDF, synthPUMS.h, coords)
  return(synthPUMS.h)
}

#FUNCTION: Takes a single dataframe with SERIAL column and recodes it into a new column, SERIAL_SYNTH
#INPUTS:  synthPUMS.h (dataframe)
#OUTPUTS:  synthPUMSout.h (data frame) telling us which instance of the serial it is
#REQUIREMENTS:  plyr
makeSerialSynth<-  function(synthPUMS.h, isUSA){
  suppressMessages(require(plyr))
  if (isUSA){
    synthPUMSout.h <- arrange(synthPUMS.h, SERIAL)
    serialSuffix <- unlist(sapply(table(synthPUMSout.h$SERIAL), function(n) 1:n), use.names=FALSE)
    synthPUMSout.h$serialSynth <- paste0(synthPUMSout.h$SERIAL, "-", serialSuffix)
    return(synthPUMSout.h)
  }
  synthPUMSout.h<- arrange(synthPUMS.h, SERIAL)
  serialSuffix = unlist(sapply(table(synthPUMSout.h$SERIAL), function(n) 1:n), use.names=FALSE)
  synthPUMSout.h$serialSynth<-synthPUMSout.h$SERIAL + serialSuffix
  return(synthPUMSout.h)
}

#FUNCTION:  assemble the people synthetic data into a data frame using the SERIAL from the household synthetic data
#INPUTS:  stdPUMS.p (data frame), synthPUMS.h (data frame), params (list)
#OUTPUTS:  data frame of synthetic people
#REQUIREMENTS:  addExtraVars, plyr
#Notes:  Assumes  stdPUMS.p has a uniID of pasted together geoLevels, must have a serial column in both

assembleSynthPUMS.p<- function(stdPUMS.p, synthPUMS.h, params, geoLevel, 
                               isUSA, extraVarsData, inputFolder){
  #join the families from the stdPUMS.p to the synthetic households
  suppressMessages(require(plyr))
  varsToKeep.p <- params$varsToKeepP
  if (isUSA) {
    regions<- unique(synthPUMS.h$PUMA)
  } else {
    regions<- unique(pasteGeoLevels(synthPUMS.h, geoLevel))
  }
  # commenting these out because it doesn't feel necessary
  # subsettedInds <- subsetPUMS(stdPUMS.p, regions, type = "p", params$hasPUMS, isUSA)
  # stdPUMS.p <- stdPUMS.p[subsettedInds,]
  pumsSub.p <- subset(stdPUMS.p, select=varsToKeep.p)
  
  if (isUSA) {
    synthPUMS.p <- join(synthPUMS.h, pumsSub.p, by ="SERIAL")
  }
  
  #add in extra variables if directed to do so
  extraVars<- params$addExtraVars
  if (extraVars) {

    # workplaces
    source(paste0(inputFolder, "functions/workplaces_assign_v3.R"))
    synthPUMS.p$WorkID<- getWorkplace(1:nrow(synthPUMS.p), synthPUMS.p$PUMA, 
                                      extraVarsData$workPlaceDF, extraVarsData$lookUpTable, geoID, 
                                      paste0(synthPUMS.p$geoLevel0[1], synthPUMS.p$geoLevel1[1]))
    # schools
    source(paste0(inputFolder, "functions/schools_assign_v3.R"))
    synthPUMS.p$SchoolID<- getSchool(synthPUMS.p, extraVarsData$schoolDF.pub, 
                                     extraVarsData$schoolDF.priv)
  }
  return(synthPUMS.p)
}

#FUNCTION:  add extra variables such as schools, workplaces, etc. to the current synth pop
#INPUTS:  synthPUMS.p (data frame), params (list)
#OUTPUTS:  data frame with #cols equal to the number of variables we're adding and same number of rows as synthPUMS.p
#TODO:  build this function

addExtraVars<- function(synthPUMS.p, params){
  return(TRUE)
  stop("The addExtraVars function is under construction!")
  return(FALSE)
}

#FUNCTION:  do weighted sampling by solving KKT conditions using hhAve
#INPUTS:  count (int), subsettedPUMSInds.h (int vector), hhAve (real number), stdPUMS.h (df)
#OUTPUTS:   vector of indices
#REQUIREMENTS:
#TODO:  Build this function
weightedSamplingKKT<- function(count, subsettedPUMSInds.h, hhAve, stdPUMS.h){
  # Load package for solving the quadratic program
  require(quadprog)
  
  # Obtain probability weights to sample 
  # from household sizes ---------------
  
  # Get unique household sizes 
  householdSizes<- sort(unique(stdPUMS.h$PERSONS))
  
  # Get probabilities for sampling sampling each household size
  n <- householdSizes #distinct household sizes
  N <- length(n) # total number of distinct household sizes
  M <- hhAve #average household size
  Q <- diag(N)
  
  # constraint matrix (4 equality, 1 ineq), must be in form of transpose for some reason
  A <- t(rbind(rep(1, N), n, diag(N)))#, rep(1,N)) 
  dvec <- rep(0,N) #there is no d^Tx term to minimize
  b <- c(1, M, rep(0,N))
  meq <- 2 #first two constraints are equality constraints
  sol <- solve.QP(Dmat=Q, dvec=dvec, Amat=A, bvec=b, meq=2)
  x <- sol$solution
  x <- ifelse(x < 0, 0, x) #Truncate the solution values to 0 if they turn into negatives. 
  
  # Get the individual Probabilities for every household record in PUMS
  indProbs <- as.numeric(x/table(stdPUMS.h$PERSONS))
  
  # create dataframe with household size and individual probability 
  hhProbs <- data.frame(hhSize=n, weights=indProbs)
  
  # Merge the probabilities onto the sorresponding PUMS microdata
  personRecords <- data.frame(stdPUMS.h$PERSONS)
  personRecords$id = 1:nrow(personRecords)
  colnames(personRecords) <- c("pumsRecords", "id")
  personRecordsMerge <- merge(personRecords, hhProbs, by.x="pumsRecords", by.y= "hhSize", all.x = TRUE, sort = FALSE)
  personRecordsMergeSort <- personRecordsMerge[order(personRecordsMerge$id),]
  individualProbs <- personRecordsMergeSort$weights
  
  # Sample from the microdata using the probabilities, and resturn the indices
  randRows <- sample(subsettedPUMSInds.h, count, replace=TRUE, prob=individualProbs)
  return(randRows)
}

#FUNCTION:  proper File naming scheme for a given country with its parameters
#INPUTS:  regionRow (int), popTable (DF), sampling Scheme (char)
#OUTPUTS:  fileName
#REQUIREMENTS:  NONE
#Example Bo, Sierra Leone year 2009 version 2, sampling scheme unif, geolevel 1 becomes
# "2009sierraleone1_unif_sierra_leone_bo"
makeFileName<- function(ID1, ID2, samplingScheme, geoLevel, countryName, year, version) {
  uniID2 <- gsub(" ", "_", ID2)
  uniID1 <- gsub(" ", "_", ID1)
  fileName <- paste0(year, stripWhitePunct(countryName), geoLevel, 
                    version, "_", samplingScheme, "_", uniID1, "_", 
                    uniID2)
  return(fileName)
}

#FUNCTION:  write the synthetic data for a particular region to outputFolder as .csv
#INPUTS:  pums (df), fileName (char), outputFolder (char), type (char)
#OUTPUTS:  written out .csv and a logical
#REQUIREMENTS:  NONE
writePUMS <- function(ID1, ID2 = NULL, pums, fileName, outputFolder, type) {  
  if (!file.exists(paste0(outputFolder, ID1, "/"))) {
    dir.create(paste0(outputFolder, ID1, "/"))
  }
  path <- paste0(outputFolder, ID1, "/", fileName, "_",  type, ".csv")
  write.csv(pums, path)
  return(TRUE)
}


