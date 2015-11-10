# 2015 January 15 Main Function V2 
# Shannon Gallagher, Lee Richardson 
# DESCRIPTION: The main run file for creating synthetic populations

# FUNCTION: Main run of creating synthetic populations INPUTS:
# countryName (char), year (4 digit int), geoLevel (int), version (string
# 'VX'), user (string) OUTPUTS: logical along with synthetic household
# .csvs, synthetic people .csvs, and a metadata file REQUIREMENTS:
# FUNCTIONS IN SAME .R: areInputsGood, callUserParams, callCountryParams,
# formatPUMS, checkForHOles, fillInHoles SOURCED FILES:
# miscFunctions_v2.R, WASE_configFile_v2.R, standardizeGeoLevels_v2.R,
# counts_v2.R, makingMicroData_v2.R ATTN: config file needs to be written
# and this needs to be tested

mainRun <- function(countryName, censusYear = 2000, geoLevel = 1, version = "v2", 
                    user = "shannon", inputFolder = "~/midas_v2/", isUSA = FALSE, 
                    geoID = "42003", samplingMethod = "unif", append = TRUE) {
  # format high level inputs first call parameters, user params and country
  # params then load in the appropriate shapefile, format it load in
  # appropriate popTable, format it check for holes, modify popTable and
  # shapefile if necessary make microData with the country and
  # userParameters, shapefiles, and popTables return TRUE
  suppressMessages(require(data.table))  # for readMicroData fread function
  source(paste0(inputFolder, "functions/miscFunctions_", version, ".R"))
  countryName <- stripWhitePunct(countryName)
  
  # call the configs
  print("Setting the Configs")
  source(paste0(inputFolder, "configFiles/configs_v2.R"))
  userParams <- callUserParams(user, countryName)
  countryParams <- callCountryParams(countryName, year, geoLevel, version, 
                                     isUSA = isUSA, geoID = geoID)
  geohiveLevel <- countryParams[["geohiveLevel"]]
  
  # writing a log file
  print("Writing Logfile")
  outputFolder <- countryParams$outputFolder
  outfileName <- paste0(outputFolder, "outfile_", countryName, ".txt")
  countryTxt <- paste0("Country: ", countryName)
  yearTxt <- paste("Year:", censusYear)
  geolevelTxt <- paste("Geo Level:", geoLevel)
  versionTxt <- paste("Version:", version)
  userTxt <- paste("User:", user)
  time <- Sys.time()
  writeLines(c(countryTxt, yearTxt, geolevelTxt, versionTxt, userTxt, time, 
               "\n"), outfileName)
  file.append(outfileName, paste0(outputFolder, "outfile.txt"))
  
  # set a seed
  set.seed(countryParams$seed)
  numDecLatLong <- countryParams$numDecLatLong
  
  # Source the loading data functions for shapefile and pums.p then format
  # and sort the PUMS and shapefiles to have the appropriate geolevels
  print("Loading and Standardizing PUMS")
  source(paste0(inputFolder, "functions/WASE_configFile_", version, ".R"))
  source(paste0(inputFolder, "functions/standardizeGeoLevels_", version, ".R"))
  
  # Load in and standardize the people level microdata
  pums.p <- readMicroData(inputFolder, countryParams$PUMScountry, countryName, 
                          isUSA, countryParams$PUMS.p, countryParams$dataFolder)
  stdPUMS.p <- stdizePUMS(inputFolder, pums.p, countryName, countryParams$hasPUMS, 
                          geoLevel, countryParams$year, countryParams)
  if (isUSA) {
    colnames(stdPUMS.p)[which(colnames(stdPUMS.p) == "geoLevel1")] <- "PUMA"
    colnames(stdPUMS.p)[which(colnames(stdPUMS.p) == "SERIALNO")] <- "SERIAL"
  }
  
  # Load and standardize the household level data
  pums.h <- readMicroData(inputFolder, countryParams$PUMScountry, countryName, 
                          isUSA, countryParams$PUMS.h, countryParams$dataFolder)
  stdPUMS.h <- stdizePUMS(inputFolder, pums.h, countryName, countryParams$hasPUMS, 
                          geoLevel, countryParams$year, countryParams)
  
  if (isUSA) {
    colnames(stdPUMS.h)[which(colnames(stdPUMS.h) == "geoLevel1")] <- "PUMA"
    colnames(stdPUMS.h)[which(colnames(stdPUMS.h) == "SERIALNO")] <- "SERIAL"
  }
  
  # Only use the SERIAL Numbers which have a corresponding ID coming from
  # the household survey. In the united states, this means removing all of
  # the 0 person households. Although this sounds strange, the household
  # PUMS has records where NP (number persons) equals 0
  pser <- unique(stdPUMS.p$SERIAL)
  stdPUMS.h <- stdPUMS.h[stdPUMS.h$SERIAL %in% pser, ]
  
  # add the geoLevel names to the shapefile
  print("Loading and Standardizing Shapefiles")
  shapefile <- readGeography(inputFolder, countryParams$geographyFileName, 
                             countryParams$shapeFileType, countryName, isUSA, 
                             countryParams$dataFolder)
  p.slot <- slot(shapefile, "plotOrder")
  stdShapefile <- stdizeShapefile(inputFolder, countryName, countryParams$hasPUMS, 
                                  shapefile, numDecLatLong, geoLevel, countryParams$year, countryParams, 
                                  isUSA, countryParams$dataFolder)
  if (!countryParams$hasPUMS & geoLevel) {
    stdShapefile <- unionRegions(stdShapefile, geoLevel)
  }
  
  # load in the population Table
  print("Loading and merging the Region Counts")

  source(paste0(inputFolder, "functions/counts_", version, ".R"))
  popTable <- getRegionCountsDF(countryName, countryParams$year, geoLevel, 
                                stdPUMS.h, countryParams, stdShapefile, inputFolder, version, userParams, 
                                geohiveLevel, countryParams$dataFolder, countryParams$geoID)
  emptyTracts = which(popTable$counts == 0)
  browser()  
  # write the pop table
  write.csv(popTable, paste0(outputFolder, "popTableTxt.csv"), row.names = F)
  file.append(outfileName, paste0(outputFolder, "popTableTxt.csv"))
  
  # check for holes and if join together if necessary
  areHoles <- checkForHoles(stdShapefile)
  if (areHoles) {
    noHoles <- fillInHoles(stdShapefile, popTable)
    stdShapefile <- noHoles$stdShapefile
    popTable <- noHoles$stdShapefile
  }
  # load in schools and workplaces!
  print("Schools and Workplaces")
  workPlaceDF <- NULL
  schoolDF.pub <- NULL
  schoolDF.priv <- NULL
  lookUpTable <- NULL
  if (countryParams$addExtraVars) {
    source(paste0(inputFolder, "functions/readOtherData.R"))
    schoolDF.pub <- readSchoolData(countryName, countryParams$dataFolder, 
                                   type = "public")
    schoolDF.priv <- readSchoolData(countryName, countryParams$dataFolder, 
                                    type = "private")
    workPlaceDF <- readWorkplaceData(countryName, countryParams$dataFolder)
    bad_ids <- which(nchar(workPlaceDF$stcotr) < 10)
    workPlaceDF <- workPlaceDF[-bad_ids, ]
    stcotr <- t(sapply(as.character(workPlaceDF$stcotr), breakUpFPS))
    stcotr.df <- data.frame(st = stcotr[, 1], co = stcotr[, 2], tr = stcotr[, 
                                                                            3])
    workPlaceDF <- cbind(workPlaceDF, stcotr.df)
    lookUpTable <- read.csv(paste0(countryParams$dataFolder, "tables/lookup00_v3.csv"))
    extraVarsData <- list(schoolDF.pub = schoolDF.pub, schoolDF.priv = schoolDF.priv, 
                          workPlaceDF = workPlaceDF, lookUpTable = lookUpTable)
  }
  
  # Run the functions which output microdata from each one of our
  # geographical regions of interest
  print("Making Microdata")
  source(paste0(inputFolder, "functions/makingMicroData_", version, ".R"))
  didRun <- outputMicroData(countryParams, stdPUMS.p, stdPUMS.h, stdShapefile, 
                            popTable, samplingScheme = samplingMethod, inputFolder, countryParams$outputFolder, 
                            geoLevel, countryName, countryParams$hasPUMS, countryParams$year, 
                            version, userParams$codeFolder, numDecLatLong, isUSA, extraVarsData, 
                            dataFolder = countryParams$dataFolder)
  
  # Decide whether or not to append all of the household or person level
  # files together after generating them.  if (append == TRUE) { # Get a
  # list of the final household and person level # synthetic population
  # files outputFiles <- list.files(paste0(countryParams$outputFolder,
  # 'synth_pops/')) h_files = outputFiles[grep('_h.csv', outputFiles)]
  # p_files = outputFiles[grep('_p.csv', outputFiles)] # Run the append
  # files fundtion on both the person # and household lists in order to
  # save the large # data-frame in one location appendFiles(fileList =
  # h_files, outputFolder = countryParams$outputFolder, type = 'h')
  # appendFiles(fileList = p_files, outputFolder =
  # countryParams$outputFolder, type = 'p') }
  return(didRun)
}

# FUNCTION: formats the PUMS by naming the geoLevels correctly and
# sorting them by geoLevel and adds a uniID column INPUTS: inputFolder
# (char), pums.p (data frame), countryName (char), hasPUMS (logical),
# geoLevel (int), year (4 digit int), params (list) OUTPUTS: pumsOut
# (data frame) REQUIREMENTS: standardizeGeoLevels_v2.R,
# miscFunctions_v2.R

formatPUMS <- function(inputFolder, pums.p, countryName, hasPUMS, geoLevel, 
                       year, params) {
  # add the geoLevel names, add the fullID, sort the PUMS
  stdPUMS <- stdizePUMS(inputFolder, pums.p, countryName, hasPUMS, geoLevel, 
                        year, params)
  stdPUMS$uniID <- pasteGeoLevels(stdPUMS, geoLevel)
  stdPUMS <- sortPUMS(stdPUMS, geoLevel, hasPUMS)
  return(stdPUMS)
}

# FUNCTION: check the shapefile for holes INPUTS: shapefile OUTPUTS:
# logical REQUIREMENTS: TODO: COMPLETE THIS FUNCTION

checkForHoles <- function(stdShapefile) {
  return(FALSE)
}

# FUNCTION: fill in the holes of a shapefile INPUTS: stdShapefile
# (shapefile), popTable (data frame) OUTPUTS: list with the filled in
# stdShapefile and popTable REQUIREMENTS:

fillInHoles <- function(stdShapefile, popTable) {
  stop("The fillInHoles Function is under construction!")
  return(TRUE)
}

# FUNCTION: breakUPFPS number into character df INPUTS: stcotr (11 or 10
# digit num) OUTPUTS: df
breakUpFPS <- function(stcotr) {
  stcotr <- as.character(stcotr)
  if (nchar(stcotr) == 10) {
    st <- substr(stcotr, 1, 1)
    co <- substr(stcotr, 2, 4)
    tr <- substr(stcotr, 5, 10)
  } else if (nchar(stcotr) == 11) {
    st <- substr(stcotr, 1, 2)
    co <- substr(stcotr, 3, 5)
    tr <- substr(stcotr, 6, 11)
  } else {
    stop("FPS isn't of appropriate length!")
  }
  df <- c(st, co, tr)
  return(df)
}

# FUNCTION: Append all the either household or people level synthpop's
# together so that we can look at the summary statistics INPUTS: fileList
# (character vector) outputFolder (character of filepath to save the
# appended files) OUTPUTS: one CSV which has an appended synthetic
# population for all of the individual regions
appendFiles <- function(fileList, outputFolder, type) {
  print("inside the append files function")
  for (file in fileList) {
    if (file == fileList[1]) {
      combined <- read.csv(file)
    } else {
      tmp <- read.csv(file)
      combined <- append(combined, tmp)
    }
  }
  write.csv(combined, paste0(outputFolder, "combined_", type, ".csv"))
  return(combined)
}

