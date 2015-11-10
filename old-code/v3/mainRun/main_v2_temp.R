#2015 January 15
#Main Function V2
#Shannon Gallagher, Beka Steorts, Lee Richardson
#DESCRIPTION:  The main run file for creating synthetic populations

#FUNCTION:  Main run of creating synthetic populations
#INPUTS:  countryName (char), year (4 digit int), geoLevel (int), version (string "VX"), user (string); inputFolder (string); geoID (int OR character); isUSA (logical)
#OUTPUTS:  logical along with synthetic household .csvs, synthetic people .csvs, and a metadata file
#REQUIREMENTS: FUNCTIONS IN SAME .R:
                  # areInputsGood, callUserParams, callCountryParams, formatPUMS, checkForHoles, fillInHoles
#SOURCED FILES:  miscFunctions_v2.R, WASE_configFile_v2.R, standardizeGeoLevels_v2.R, counts_v2.R, makingMicroData_v2.R
#ATTN:  config file needs to be written and this needs to be tested

mainRun<- function(countryName, censusYear=2000, geoLevel=1, version="v3", user="shannon", inputFolder="~/midas_v2/", geoID=NA, isUSA=FALSE, geoIDType="FIPS"){
    #format high level inputs
    #first call parameters, user params and country params
    #then load in the appropriate shapefile, format it
    #load in appropriate popTable, format it
    #check for holes, modify popTable and shapefile if necessary
    #make microData  with the country and userParameters, shapefiles, and popTables
    #return TRUE
    inputStatus<- areInputsGood(countryName, year, geoLevel, version, user)
    stopifnot(inputStatus)
    source(paste0(inputFolder,"functions/miscFunctions_", version, ".R"))
    countryName<- stripWhitePunct(countryName)
    if(!is.na(geoID)){    geoID<- stripWhitePunct(geoID)}
    #call the configs
    source(paste0(inputFolder, "configFiles/configs_", version, ".R"))
    userParams<- callUserParams(user)
    countryParams<- callCountryParams(countryName, year, geoLevel, version, isUSA, geoID, geoIDType)
    set.seed(countryParams$seed)
    numDecLatLong<- countryParams$numDecLatLong
    #source the loading data functions for shapefile and pums.p
    source(paste0(inputFolder, "functions/WASE_configFile_", version, ".R"))
    shapefile<- readGeography(inputFolder, countryParams$geographyFileName, countryParams$shapeFileType, countryName)
    pums.p<- readMicroData(inputFolder, countryParams$PUMScountry, countryName)
    #formatting and sorting the PUMS and shapefiles to have appropriate geoLevels
    source(paste0(inputFolder, "functions/standardizeGeoLevels_", version, ".R"))
    stdPUMS.p<- stdizePUMS(inputFolder, pums.p, countryName, countryParams$hasPUMS, geoLevel, countryParams$year, countryParams)
    #ATTN:  THE BELOW LINE NEEDS TESTED
    stdPUMS.h<- stdPUMS.p[!duplicated(stdPUMS.p$SERIAL),] #extracts the unique households
    #add the geoLevel names to the shapefile
    stdShapefile<- stdizeShapefile(inputFolder, countryName, countryParams$hasPUMS, shapefile, numDecLatLong, geoLevel,  countryParams$year, countryParams)
    #TODO:  2/7/2015 SKG:  fix the unionRegion functions for v2
    #UPDATE:  2/22/15 SKG:  this is fixed for v2
    if(!countryParams$hasPUMS & geoLevel){
        stdShapefile<- unionRegions(stdShapefile, geoLevel)
    }
    #load in the population Table
    source(paste0(inputFolder, "functions/counts_", version, ".R"))
    #ATTN:  a function in getRegionCounts still needs to be written (matching the region ID order with the shapefile order)
    popTable<- getRegionCountsDF(countryName, countryParams$year, geoLevel, stdPUMS.h, countryParams, stdShapefile, inputFolder, version, userParams)
    #check for holes and if join together if necessary
    areHoles<- checkForHoles(stdShapefile)
    if (areHoles){
        noHoles<- fillInHoles(stdShapefile, popTable)
        stdShapefile<- noHoles$stdShapefile; popTable<- noHoles$stdShapefile
    }
    #run the damn thing
    source(paste0(inputFolder, "functions/makingMicroData_", version, ".R"))
    didRun<- outputMicroData(countryParams, stdPUMS.p, stdPUMS.h, stdShapefile, popTable, samplingScheme="unif", inputFolder, userParams$outputFolder, geoLevel, countryName, countryParams$hasPUMS, countryParams$year, version, userParams$codeFolder, numDecLatLong)
    return(didRun)
}


#FUNCTION:  checks if the inputs to the MainRun are valid
#INPUTS:  countryName (char), year (4 digits), geoLevel (int), version (string), user (string)
#OUTPUTS:  logical
#TODO:  Write this function!!

areInputsGood<- function(countryName, year, geoLevel, version, user){
  return(TRUE)  
}


#FUNCTION:  formats the PUMS by naming the geoLevels correctly and sorting them by geoLevel and adds a uniID column
#INPUTS:  inputFolder (char), pums.p (data frame), countryName (char), hasPUMS (logical), geoLevel (int), year (4 digit int), params (list)
#OUTPUTS: pumsOut (data frame)
#REQUIREMENTS:  standardizeGeoLevels_v2.R, miscFunctions_v2.R

formatPUMS<- function(inputFolder, pums.p, countryName, hasPUMS, geoLevel, year, params){
    #add the geoLevel names, add the fullID, sort the PUMS
    stdPUMS<- stdizePUMS(inputFolder, pums.p, countryName, hasPUMS, geoLevel, year, params)
    stdPUMS$uniID<- pasteGeoLevels(stdPUMS, geoLevel)
    stdPUMS<- sortPUMS(stdPUMS, geoLevel, hasPUMS)
    return(stdPUMS)
}

#FUNCTION:  check the shapefile for holes
#INPUTS:  shapefile
#OUTPUTS:  logical
#REQUIREMENTS:
#TODO:  COMPLETE THIS FUNCTION

checkForHoles<-function(stdShapefile){
    return(FALSE)
}

#FUNCTION:  fill in the holes of a shapefile
#INPUTS:  stdShapefile (shapefile), popTable (data frame)
#OUTPUTS: list with the filled in stdShapefile and popTable
#REQUIREMENTS:  

fillInHoles<- function(stdShapefile, popTable){
    stop("The fillInHoles Function is under construction!")
    return(TRUE)

}

## Call the function
mainRun(countryName="thegambia", censusYear=2000, geoLevel=2, version="v2", 
        user="lee2", inputFolder="C:/Users/leeri_000/midas_v2/")



