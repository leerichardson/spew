#2015 January 15
#Shannon Gallagher, Beka Steorts, Lee Richardson
#Configuration File
#User and Country configs

#FUNCTION:  call the User parameters
#ATTN: 
#INPUTS:  user (char)
#OUTPUTS:  userParams (list); only outputFolders right now
#REQUIREMENTS:  NONE

callUserParams<- function(user, country){
  switch(user,
         v2_user = {
           codeFolder<- "functions/"
         },
         shannon = {  
           if(file.exists(paste0("/media/shannon/TOSHIBA\ EXT/ebola_outputs/", country, "/"))){
             unlink(paste0("/media/shannon/TOSHIBA\ EXT/ebola_outputs/", country, "/"), recursive=TRUE)
             dir.create(paste0("/media/shannon/TOSHIBA\ EXT/ebola_outputs/", country, "/"))
             outputFolder<- paste0("/media/shannon/TOSHIBA\ EXT/ebola_outputs/", country, "/")
           } else {
             dir.create(paste0("/media/shannon/TOSHIBA\ EXT/ebola_outputs/", country, "/"))
             outputFolder<- paste0("/media/shannon/TOSHIBA\ EXT/ebola_outputs/", country, "/")                      
           } 
           codeFolder<- "functions/"    
         },
         shannon2 = {outputFolder<- "~/Desktop/ebola_outputs/"
                     codeFolder<- "functions/"
         },
         stop("Enter a valid user.")
  )
  userParams<- list(codeFolder=codeFolder)
  return(userParams)
}

#FUNCTION:  call country parameters which include
# year (4 digit int), geographyFileName (name of shapefile to use), 
# shapeFileType (what source is shapefile from), PUMScountry (what's the surrogate pums), 
# PUMS (char vec names of geolev0, geolevel1, etc.), hasPUMS (logical, 
# does this country have PUMS from IPUMS,  addExtraVars (logical), 
# numDecLatLong (int), varsToKeepP (char vec), varsToKeepH (char vec), 
# shapefile (char vec names of geolev0, geolevel1, etc.), hhAve (real number)
#INPUTS:  countryName (char), year (currently useless), geoLevel (int), version (char "vX")
#OUTPUTS:  countryParams (list) details above
#REQUIREMENTS:  NONE

callCountryParams<- function(countryName, year, geoLevel, version, isUSA=FALSE, geoID=NA){
  PUMS.h<-"NULL"; PUMS.p<-"NULL"; PUMS<- "NULL"   ; hhAve<- "NULL";geohiveLevel = "NULL"; countsGeoNames<- "NULL";
  #for USA, call new function to get those parameters
  if (isUSA){
    countryParams<- getUSAParams(countryName, year, geoLevel, version, isUSA, geoID, geoIDType)
    return(countryParams)
  }
  
  # Western Africa ---------------------
  hhAve<- "NULL"
  geohiveLevel = "NULL"
  countsGeoNames<- "NULL"
  if (countryName=="sierraleone" & geoLevel==1 & version=="v2"){
    countryCode<-"SL"
    countryNum<- 694
    year<- 2000
    geographyFileName<- "world_geolev1.shp"
    shapeFileType<- "world"
    PUMScountry<-  "ipumsi_00001_694.RData"
    hasPUMS<- TRUE
    addExtraVars<- FALSE
    shapefile<- c("CNTRY_NAME", "GEOLEVEL1")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "PUMS"
  }
  #liberia gl 1, v2
  else if (countryName == "liberia" & geoLevel==1 & version=="v2"){
    countryCode<-"LR"
    year<- 2000
    geographyFileName<- "world_geolev1.shp"
    shapeFileType<- "world"
    PUMScountry<-  "ipumsi_00001_430.RData"
    hasPUMS<- TRUE
    addExtraVars<- FALSE
    shapefile<- c("CNTRY_NAME", "GEOLEVEL1")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "PUMS"
  }
  #guinea, gl 1, v2
  else if (countryName == "guinea" & geoLevel==1 & version=="v2"){
    countryCode<-"GN"
    year<- 2000
    geographyFileName<- "world_geolev1.shp"
    shapeFileType<- "world"
    PUMScountry<-  "ipumsi_00001_324.RData"
    hasPUMS<- TRUE
    addExtraVars<- FALSE
    
    shapefile<- c("CNTRY_NAME", "GEOLEVEL1")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "PUMS"
  }
  #senegal, gl1, v2
  else if (countryName == "senegal" & geoLevel==1 & version=="v2"){
    countryCode<-"SN"
    year<- 2000
    geographyFileName<- "world_geolev1.shp"
    shapeFileType<- "world"
    PUMScountry<-  "ipumsi_00001_686.RData"
    hasPUMS<- TRUE
    addExtraVars<- FALSE
    shapefile<- c("CNTRY_NAME", "GEOLEVEL1")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "PUMS"
  }
  #nigeria, gl 1, v2, currently non functional, needs a popTable
  else if (countryName == "nigeria" & geoLevel==1 & version=="v2"){
    countryCode<-"NG"
    year<- 2000
    geographyFileName<- "world_geolev1.shp"
    shapeFileType<- "world"
    PUMScountry<-  "ipumsi_00001_430.RData" #liberia
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("CNTRY_NAME", "GEOLEVEL1")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "pretab"
  }
  #mali, gl1, v2, non functional as mali has holes
  else if (countryName == "mali" & geoLevel==1 & version=="v2"){
    countryCode<-"ML"
    year<- 2000
    geographyFileName<- "world_geolev1.shp"
    shapeFileType<- "world"
    PUMScountry<-  "ipumsi_mali.RData"
    hasPUMS<- TRUE
    addExtraVars<- FALSE
    shapefile<- c("CNTRY_NAME", "GEOLEVEL1")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "PUMS"
  }
  
  #countries without PUMS
  #cote d'ivoire, gl 1, v2
  else if (countryName == "cotedivoire" & geoLevel==1 & version=="v2"){
    countryCode<-"CI"
    year<- 1998
    geographyFileName<- "cotedivoire/cotedivoire.shp"
    shapeFileType<- "mapMakerUK"
    PUMScountry<- "ipumsi_00001_430.RData" #Liberia
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("geoLevel0", "ID")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    hhAve<- 5.1
    table<- "pretab"
  }
  else if (countryName == "guineabissau" & geoLevel==1 & version=="v2"){
    countryCode<-"GW"
    year<- 2009
    geographyFileName<- "guineabissau/guineabissau.shp"
    shapeFileType<- "mapMakerUK"
    PUMScountry<- "ipumsi_00001_694.RData"  #Sierra leone
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("geoLevel0", "ID")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    hhAve<- 7.6
    table<- "pretab"
  }
  else if (countryName == "thegambia" & geoLevel==1 & version=="v2"){
    countryCode<-"GM"
    year<- 2003
    geographyFileName<- "thegambia/thegambia.shp"
    shapeFileType<- "mapMakerUK"
    PUMScountry<- "ipumsi_00001_694.RData"  #Sierra leone
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("geoLevel0", "ID")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    hhAve<- 8.2        #http://www.gbos.gov.gm/uploads/census/The%20Gambia%20Population%20and%20Housing%20Census%202013%20Provisional%20Report.pdf
    table<- "pretab"
  }
  #######second level geography
  else if (countryName == "sierraleone" & geoLevel==2 & version=="v2"){
    countryCode<-"SL"
    countryNum<- 694
    year<- 2004
    #change this one
    geographyFileName<- "sierraleone/SLE_adm3.shp"
    #change this 
    shapeFileType<- "gadm"
    PUMScountry<-  "ipumsi_00001_694.RData"#sierra leone
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0", "NAME_2", "NAME_3")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "geohive"
    geohiveLevel <- "level4"
    hhAve<- 10 #source:   #http://home.wfp.org/stellent/groups/public/documents/ena/wfp216299.pdf
    countsGeoNames<- c(NA, NA, "ProvinceDistrict")
    
  }
  else if ( countryName== "liberia" & geoLevel==2 & version=="v2"){
    # stop("We dont' have second level counts for Liberia!")
    countryCode<-"LR"
    countryNum<- 430
    year<- 2008
    geographyFileName<- "liberia/LBR_adm2.shp"
    shapeFileType<- "gadm"
    PUMScountry<-  "ipumsi_00001_430.RData"
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0", "NAME_1", "NAME_2")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "geohive"
    geohiveLevel <- "level4"
    hhAve<- 5.1 #source : http://www.aho.afro.who.int/profiles_information/index.php/Liberia:Analytical_summary_-_Social_determinants
    countsGeoNames<- c(NA, NA, "countydistrict")
  }
  else if (countryName == "guinea" & geoLevel==2 & version=="v2"){
    #stop("We are in the process of scraping Guinea's geohive data")
    countryCode<-"GN"
    countryNum<- 324
    year<- 2014
    geographyFileName<- "guinea/GIN_adm2.shp"
    shapeFileType<- "gadm"
    PUMScountry<-  "ipumsi_00001_324.RData"
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0", "NAME_1", "NAME_2")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "geohive"
    geohiveLevel = "level2"
    hhAve<- 8 #http://www.encyclopedia.com/topic/Equatorial_Guinea.aspx
    countsGeoNames<- c(NA, NA, "regionprefecture")
  }
  else if (countryName == "senegal" & geoLevel==2 & version=="v2"){
    # stop("We need to first get rid of special characters in the shapefile!")
    countryCode<-"SN"
    countryNum<- 686
    year<- 2013
    geographyFileName<- "senegal/SEN_adm2.shp"
    shapeFileType<- "gadm"
    PUMScountry<-  "ipumsi_00001_686.RData"
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0", "NAME_1", "NAME_2")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "geohive"
    geohiveLevel <- "level2"
    hhAve<- 8.4 #http://www.worldbank.org/transport/transportresults/regions/africa/senegal-output-eng.pdf
    countsGeoNames<- c(NA, NA, "regiondepartmentarrondissementcommune")
  }
  else if (countryName == "mali" & geoLevel==2 & version=="v2"){
    #stop("Mali needs special attention to its varsToKeep.p")
    countryCode<-"ML"
    year<- 2009
    geographyFileName<- "mali/MLI_adm2.shp"
    shapeFileType<- "gadm"
    PUMScountry<-  "ipumsi_mali.RData"
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0","NAME_1", "NAME_2")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    table<- "geohive"
    geohiveLevel <- "level2"
    countsGeoNames<- c(NA, NA, "regioncercle")
    hhAve<- 5.3 #http://dhsprogram.com/pubs/pdf/SR95/Ml01KeyFindings.pdf
  }
  else if (countryName == "cotedivoire" & geoLevel==2 & version=="v2"){
    # stop("Cote d'Ivoire needs foreign characters removed from its shapefiles.")
    countryCode<-"CI"
    year<- 1998
    geographyFileName<- "cotedivoire/CIV_adm2.shp"
    shapeFileType<- "gadm"
    PUMScountry<- "ipumsi_00001_430.RData" #Liberia
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0","NAME_1", "NAME_2")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    hhAve<- 5.1
    table<- "geohive"
    geohiveLevel <- "NULL"
    countsGeoNames<- c(NA, NA, "regiondepartement")
  }
  else if (countryName == "guineabissau" & geoLevel==2 & version=="v2"){
    #   stop("Guinea-Bissau  needs foreign characters removed from its shapefiles.")
    countryCode<-"GW"
    year<- 2009
    geographyFileName<- "guineabissau/GNB_adm2.shp"
    shapeFileType<- "gadm"
    PUMScountry<- "ipumsi_00001_694.RData"  #Sierra leone
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0", "NAME_1", "NAME_2")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    hhAve<- 7.6
    table<- "geohive"
    geohiveLevel <- "level2"
    countsGeoNames<- c(NA, NA, "regiaosector")        
  }
  else if (countryName == "thegambia" & geoLevel == 2 & version == "v2"){
    countryCode<-"GM"
    year<- 2003
    geographyFileName<- "thegambia/GMB_adm2.shp"
    shapeFileType<- "gadm"
    PUMScountry<- "ipumsi_00001_694.RData"  #Sierra leone
    hasPUMS<- FALSE
    addExtraVars<- FALSE
    shapefile<- c("NAME_0", "NAME_1", "NAME_2")
    PUMS<- c("CNTRY_NAME", "GEOLEV1")
    hhAve<- 8.2 #http://www.gbos.gov.gm/uploads/census/The%20Gambia%20Population%20and%20Housing%20Census%202013%20Provisional%20Report.pdf
    table<- "geohive"
    geohiveLevel = "level2"
    countsGeoNames<- c(NA, NA, "LGAdistrict") 
  } 

  # Western Hemisphere ----------------------------------
  else if (countryName == "canada" & geoLevel == 2 & version == "v3") {
    dataFolder <- paste0("/data/shared_group_data/syneco/input/west/north_america/", countryName, "/")
    outputFolder <- paste0("/data/shared_group_data/syneco/input/west/north_america/", countryName, "/")
    if (Sys.getenv("USER") == "lee") {
      dataFolder <- paste0("/home/lee/Dropbox/research/midas/", countryName, "/")
      outputFolder <- paste0("/home/lee/Dropbox/research/midas/outputs/", countryName, "/")
    }
    
    countryCode <-"CAN"
    year <- 2013
    geographyFileName <- "CAN_adm2.shp"
    shapeFileType <- "gadm"
    PUMScountry <- "ipumsi_00028.csv"
    hasPUMS <- TRUE
    addExtraVars <- FALSE
    shapefile <- c("NAME_0", "NAME_1", "NAME_2")
    PUMS <- c("CNTRY_NAME", "GEOLEV1")
    hhAve <- 5
    table <- "geohive"
    geohiveLevel <- "level2"
    countsGeoNames <- c(NA, NA, "LGAdistrict") 
  }
  
  else{
    stop("Choose a valid combination of country, geoLevel, and version.")
  }
  
  numDecLatLong <- 8
  seed <- 19
  if(countryName != "mali") {
    
    varsToKeepH <- c("CNTRY", "YEAR", "SERIAL", "PERSONS","GQ", "URBAN", 
                     "HHTYPE")
    varsToKeepP <-c("CNTRY", "YEAR", "SERIAL", "PERNUM", "WTPER",  "AGE", 
                   "SEX", "MARST", "SCHOOL", "EMPSTAT", "OCCISCO", "DISABLE", 
                   "SERIAL")
  } else {
    varsToKeepH <-c("CNTRY", "YEAR", "SERIAL", "PERSONS",
                    "GQ", "URBAN", "HHTYPE")
    varsToKeepP <-c("CNTRY", "YEAR", "SERIAL", "PERNUM", "WTPER",
                    "AGE", "SEX", "MARST")
  }
  
  countryParams<- list(countryCode=countryCode, year = year, geographyFileName=geographyFileName, 
                       shapeFileType=shapeFileType, PUMScountry=PUMScountry, hasPUMS=hasPUMS, 
                       addExtraVars=addExtraVars, shapefile=shapefile, PUMS=PUMS, hhAve=hhAve, 
                       table=table, numDecLatLong= numDecLatLong, varsToKeepH= varsToKeepH, 
                       varsToKeepP= varsToKeepP, seed=seed, countsGeoNames=countsGeoNames, 
                       geohiveLevel = geohiveLevel, PUMS.h=PUMS.h, PUMS.p=PUMS.p, dataFolder = dataFolder,
                       outputFolder = outputFolder)
  return(countryParams)   
}


# United States -------------------------
getUSAParams<- function(countryName, year, geoLevel, version, isUSA, geoID, geoIDType="FIPS") {

  # Set the folder where all the United States inputs are contained on 
  # Olympus, along with initializing some of the other parameters to NULL
  # which aren't needed for USA
  PUMS.h<-"NULL"; PUMS.p<-"NULL"; PUMS<- "NULL"; 
  hhAve<- "NULL"; geohiveLevel = "NULL"; countsGeoNames<- "NULL"; 
  hhAve<- "NULL"; geohiveLevel = "NULL"; countsGeoNames<- "NULL";
  
    
  dataFolder<- paste0("/data/shared_group_data/syneco/input/west/north_america/united_states/", geoID, "/")
  usa_table <- read.csv("/data/shared_group_data/syneco/input/west/north_america/united_states/states_lookup.csv")
  
  if (as.character(geoID) == "42003" & geoLevel==2 & version=="v3") {
    countryCode<-"PA"
    countryNum<- 42
    year<- 2010
    geographyFileName<- "tiger/tl_2010_42003_tract00.shp"
    shapeFileType<- "usa2010"
    PUMScountry<-  "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<- c("STATEFP00", "COUNTYFP00", "TRACTFP00")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10ppa.csv"
    PUMS.h<- "pums/ss10hpa.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="42" & geoLevel==2 & version=="v3"){
    countryCode<- "PA"
    countryNum<- 42
    year<- 2010
    geographyFileName<- "tiger/2010_pa/tl_2010_42_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10ppa.csv"
    PUMS.h<- "pums/ss10hpa.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID) =="06" & geoLevel==2 & version=="v3") {
    countryCode <- "CA"
    countryNum <- 06
    year <- 2010
    geographyFileName <- "tiger/2010_ca/tl_2010_06_tract10.shp"
    shapeFileType <- "usa2010"
    PUMScountry <- "NULL"
    hasPUMS <- TRUE
    addExtraVars <- TRUE
    shapefile <- c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS <- c("ST", "PUMA")
    PUMS.p <- "pums/2013/ss13pca.csv"
    PUMS.h <- "pums/2013/ss13hca.csv"
    table <- "USPUMS"
  }
  else if (as.character(geoID)=="17" & geoLevel==2 & version=="v3") {
    countryCode<- "IL"
    countryNum<- 17
    year<- 2010
    geographyFileName<- "tiger/2010_il/tl_2010_17_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/2013/ss13pil.csv"
    PUMS.h<- "pums/2013/ss13hil.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="10" & geoLevel==2 & version=="v3"){
    countryCode<- "DE"
    countryNum<- 10
    year<- 2010
    geographyFileName<- "tiger/2010_de/tl_2010_10_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pde.csv"
    PUMS.h<- "pums/ss10hde.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="44" & geoLevel==2 & version=="v3"){
    countryCode<- "RI"
    countryNum<- 44
    year<- 2010
    geographyFileName<- "tiger/2010_ri/tl_2010_44_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pri.csv"
    PUMS.h<- "pums/ss10hri.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="01" & geoLevel==2 & version=="v3"){
    countryCode<- "AL"
    countryNum<- 01
    year<- 2010
    geographyFileName<- "tiger/2010_al/tl_2010_01_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pal.csv"
    PUMS.h<- "pums/ss10hal.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="02" & geoLevel==2 & version=="v3"){
    countryCode<- "AK"
    countryNum<- 02
    year<- 2010
    geographyFileName<- "tiger/2010_ak/tl_2010_02_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pak.csv"
    PUMS.h<- "pums/ss10hak.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="04" & geoLevel==2 & version=="v3"){
    countryCode<- "AZ"
    countryNum<- 04
    year<- 2010
    geographyFileName<- "tiger/2010_az/tl_2010_04_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10paz.csv"
    PUMS.h<- "pums/ss10haz.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="05" & geoLevel==2 & version=="v3"){
    countryCode<- "AR"
    countryNum<- 05
    year<- 2010
    geographyFileName<- "tiger/2010_ar/tl_2010_05_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10par.csv"
    PUMS.h<- "pums/ss10har.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="08" & geoLevel==2 & version=="v3"){
    countryCode<- "CO"
    countryNum<- 08
    year<- 2010
    geographyFileName<- "tiger/2010_co/tl_2010_08_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pco.csv"
    PUMS.h<- "pums/ss10hco.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="09" & geoLevel==2 & version=="v3"){
    countryCode<- "CT"
    countryNum<- 09
    year<- 2010
    geographyFileName<- "tiger/2010_ct/tl_2010_09_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pct.csv"
    PUMS.h<- "pums/ss10hct.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="11" & geoLevel==2 & version=="v3"){
    countryCode<- "DC"
    countryNum<- 11
    year<- 2010
    geographyFileName<- "tiger/2010_dc/tl_2010_11_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pdc.csv"
    PUMS.h<- "pums/ss10hdc.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="12" & geoLevel==2 & version=="v3"){
    countryCode<- "FL"
    countryNum<- 12
    year<- 2010
    geographyFileName<- "tiger/2010_fl/tl_2010_12_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pfl.csv"
    PUMS.h<- "pums/ss10hfl.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="13" & geoLevel==2 & version=="v3"){
    countryCode<- "GA"
    countryNum<- 13
    year<- 2010
    geographyFileName<- "tiger/2010_ga/tl_2010_13_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pga.csv"
    PUMS.h<- "pums/ss10hga.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="15" & geoLevel==2 & version=="v3"){
    countryCode<- "HI"
    countryNum<- 15
    year<- 2010
    geographyFileName<- "tiger/2010_hi/tl_2010_15_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10phi.csv"
    PUMS.h<- "pums/ss10hhi.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="16" & geoLevel==2 & version=="v3"){
    countryCode<- "ID"
    countryNum<- 16
    year<- 2010
    geographyFileName<- "tiger/2010_id/tl_2010_16_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pid.csv"
    PUMS.h<- "pums/ss10hid.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="18" & geoLevel==2 & version=="v3"){
    countryCode<- "IN"
    countryNum<- 18
    year<- 2010
    geographyFileName<- "tiger/2010_in/tl_2010_18_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pin.csv"
    PUMS.h<- "pums/ss10hin.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="19" & geoLevel==2 & version=="v3"){
    countryCode<- "IA"
    countryNum<- 19
    year<- 2010
    geographyFileName<- "tiger/2010_ia/tl_2010_19_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pia.csv"
    PUMS.h<- "pums/ss10hia.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="20" & geoLevel==2 & version=="v3"){
    countryCode<- "KS"
    countryNum<- 20
    year<- 2010
    geographyFileName<- "tiger/2010_ks/tl_2010_20_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pks.csv"
    PUMS.h<- "pums/ss10hks.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="21" & geoLevel==2 & version=="v3"){
    countryCode<- "KY"
    countryNum<- 21
    year<- 2010
    geographyFileName<- "tiger/2010_ky/tl_2010_21_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pky.csv"
    PUMS.h<- "pums/ss10hky.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="22" & geoLevel==2 & version=="v3"){
    countryCode<- "LA"
    countryNum<- 22
    year<- 2010
    geographyFileName<- "tiger/2010_la/tl_2010_22_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pla.csv"
    PUMS.h<- "pums/ss10hla.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="23" & geoLevel==2 & version=="v3"){
    countryCode<- "ME"
    countryNum<- 23
    year<- 2010
    geographyFileName<- "tiger/2010_me/tl_2010_23_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pme.csv"
    PUMS.h<- "pums/ss10hme.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="24" & geoLevel==2 & version=="v3"){
    countryCode<- "MD"
    countryNum<- 24
    year<- 2010
    geographyFileName<- "tiger/2010_md/tl_2010_24_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pmd.csv"
    PUMS.h<- "pums/ss10hmd.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="25" & geoLevel==2 & version=="v3"){
    countryCode<- "MA"
    countryNum<- 25
    year<- 2010
    geographyFileName<- "tiger/2010_ma/tl_2010_25_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pma.csv"
    PUMS.h<- "pums/ss10hma.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="26" & geoLevel==2 & version=="v3"){
    countryCode<- "MI"
    countryNum<- 26
    year<- 2010
    geographyFileName<- "tiger/2010_mi/tl_2010_26_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pmi.csv"
    PUMS.h<- "pums/ss10hmi.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="27" & geoLevel==2 & version=="v3"){
    countryCode<- "MN"
    countryNum<- 27
    year<- 2010
    geographyFileName<- "tiger/2010_mn/tl_2010_27_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pmn.csv"
    PUMS.h<- "pums/ss10hmn.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="28" & geoLevel==2 & version=="v3"){
    countryCode<- "MS"
    countryNum<- 28
    year<- 2010
    geographyFileName<- "tiger/2010_ms/tl_2010_28_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pms.csv"
    PUMS.h<- "pums/ss10hms.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="29" & geoLevel==2 & version=="v3"){
    countryCode<- "MO"
    countryNum<- 29
    year<- 2010
    geographyFileName<- "tiger/2010_mo/tl_2010_29_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pmo.csv"
    PUMS.h<- "pums/ss10hmo.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="30" & geoLevel==2 & version=="v3"){
    countryCode<- "MT"
    countryNum<- 30
    year<- 2010
    geographyFileName<- "tiger/2010_mt/tl_2010_30_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pmt.csv"
    PUMS.h<- "pums/ss10hmt.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="31" & geoLevel==2 & version=="v3"){
    countryCode<- "NE"
    countryNum<- 31
    year<- 2010
    geographyFileName<- "tiger/2010_ne/tl_2010_31_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pne.csv"
    PUMS.h<- "pums/ss10hne.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="32" & geoLevel==2 & version=="v3"){
    countryCode<- "NV"
    countryNum<- 32
    year<- 2010
    geographyFileName<- "tiger/2010_nv/tl_2010_32_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pnv.csv"
    PUMS.h<- "pums/ss10hnv.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="33" & geoLevel==2 & version=="v3"){
    countryCode<- "NH"
    countryNum<- 33
    year<- 2010
    geographyFileName<- "tiger/2010_nh/tl_2010_33_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pnh.csv"
    PUMS.h<- "pums/ss10hnh.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="34" & geoLevel==2 & version=="v3"){
    countryCode<- "NJ"
    countryNum<- 34
    year<- 2010
    geographyFileName<- "tiger/2010_nj/tl_2010_34_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pnj.csv"
    PUMS.h<- "pums/ss10hnj.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="35" & geoLevel==2 & version=="v3"){
    countryCode<- "NM"
    countryNum<- 35
    year<- 2010
    geographyFileName<- "tiger/2010_nm/tl_2010_35_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pnm.csv"
    PUMS.h<- "pums/ss10hnm.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="36" & geoLevel==2 & version=="v3"){
    countryCode<- "NY"
    countryNum<- 36
    year<- 2010
    geographyFileName<- "tiger/2010_ny/tl_2010_36_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pny.csv"
    PUMS.h<- "pums/ss10hny.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="37" & geoLevel==2 & version=="v3"){
    countryCode<- "NC"
    countryNum<- 37
    year<- 2010
    geographyFileName<- "tiger/2010_nc/tl_2010_37_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pnc.csv"
    PUMS.h<- "pums/ss10hnc.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="38" & geoLevel==2 & version=="v3"){
    countryCode<- "ND"
    countryNum<- 38
    year<- 2010
    geographyFileName<- "tiger/2010_nd/tl_2010_38_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pnd.csv"
    PUMS.h<- "pums/ss10hnd.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="39" & geoLevel==2 & version=="v3"){
    countryCode<- "OH"
    countryNum<- 39
    year<- 2010
    geographyFileName<- "tiger/2010_oh/tl_2010_39_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10poh.csv"
    PUMS.h<- "pums/ss10hoh.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="40" & geoLevel==2 & version=="v3"){
    countryCode<- "OK"
    countryNum<- 40
    year<- 2010
    geographyFileName<- "tiger/2010_ok/tl_2010_40_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pok.csv"
    PUMS.h<- "pums/ss10hok.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="41" & geoLevel==2 & version=="v3"){
    countryCode<- "OR"
    countryNum<- 41
    year<- 2010
    geographyFileName<- "tiger/2010_or/tl_2010_41_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10por.csv"
    PUMS.h<- "pums/ss10hor.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="45" & geoLevel==2 & version=="v3"){
    countryCode<- "SC"
    countryNum<- 45
    year<- 2010
    geographyFileName<- "tiger/2010_sc/tl_2010_45_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10psc.csv"
    PUMS.h<- "pums/ss10hsc.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="46" & geoLevel==2 & version=="v3"){
    countryCode<- "SD"
    countryNum<- 46
    year<- 2010
    geographyFileName<- "tiger/2010_sd/tl_2010_46_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10psd.csv"
    PUMS.h<- "pums/ss10hsd.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="47" & geoLevel==2 & version=="v3"){
    countryCode<- "TN"
    countryNum<- 47
    year<- 2010
    geographyFileName<- "tiger/2010_tn/tl_2010_47_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10ptn.csv"
    PUMS.h<- "pums/ss10htn.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="48" & geoLevel==2 & version=="v3"){
    countryCode<- "TX"
    countryNum<- 48
    year<- 2010
    geographyFileName<- "tiger/2010_tx/tl_2010_48_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10ptx.csv"
    PUMS.h<- "pums/ss10htx.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="49" & geoLevel==2 & version=="v3"){
    countryCode<- "UT"
    countryNum<- 49
    year<- 2010
    geographyFileName<- "tiger/2010_ut/tl_2010_49_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10put.csv"
    PUMS.h<- "pums/ss10hut.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="50" & geoLevel==2 & version=="v3"){
    countryCode<- "VT"
    countryNum<- 50
    year<- 2010
    geographyFileName<- "tiger/2010_vt/tl_2010_50_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pvt.csv"
    PUMS.h<- "pums/ss10hvt.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="51" & geoLevel==2 & version=="v3"){
    countryCode<- "VA"
    countryNum<- 51
    year<- 2010
    geographyFileName<- "tiger/2010_va/tl_2010_51_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pva.csv"
    PUMS.h<- "pums/ss10hva.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="53" & geoLevel==2 & version=="v3"){
    countryCode<- "WA"
    countryNum<- 53
    year<- 2010
    geographyFileName<- "tiger/2010_wa/tl_2010_53_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pwa.csv"
    PUMS.h<- "pums/ss10hwa.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="54" & geoLevel==2 & version=="v3"){
    countryCode<- "WV"
    countryNum<- 54
    year<- 2010
    geographyFileName<- "tiger/2010_wv/tl_2010_54_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pwv.csv"
    PUMS.h<- "pums/ss10hwv.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="55" & geoLevel==2 & version=="v3"){
    countryCode<- "WI"
    countryNum<- 55
    year<- 2010
    geographyFileName<- "tiger/2010_wi/tl_2010_55_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pwi.csv"
    PUMS.h<- "pums/ss10hwi.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID)=="56" & geoLevel==2 & version=="v3"){
    countryCode<- "WY"
    countryNum<- 56
    year<- 2010
    geographyFileName<- "tiger/2010_wy/tl_2010_56_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- TRUE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10pwy.csv"
    PUMS.h<- "pums/ss10hwy.csv"
    table<- "USPUMS"
  }
  else if (as.character(geoID) == "72" & geoLevel==2 & version == "v3") {
    countryCode<- "PR"
    countryNum<- 72
    year<- 2010
    geographyFileName<- "tiger/2010_pr/tl_2010_72_tract10.shp"
    shapeFileType<- "usa2010"
    PUMScountry<- "NULL"
    hasPUMS<- TRUE
    addExtraVars<- FALSE
    shapefile<-c("STATEFP10", "COUNTYFP10", "TRACTFP10")
    PUMS<- c("ST", "PUMA")
    PUMS.p<- "pums/ss10ppr.csv"
    PUMS.h<- "pums/ss10hpr.csv"
    table<- "USPUMS"
  } else {
    stop("Choose a valid combination of country, geoLevel, and version.")
  }
  
  # Decide which of the other variable to keep when 
  # generating the synthetic population 
  numDecLatLong = 8
  varsToKeepH <- c("SERIAL", "PUMA", "WGTP", "geoLevel0", "NP")
  varsToKeepP <- c("SERIAL", "AGEP", "SEX", "MAR", "SCH", "SCHG", "RAC1P", "NATIVITY", "POBP", "HISP")
  seed <- 19
  
  # Quick change to run on my local computer
  if (Sys.getenv("USER") == "lee") {
    dataFolder <- paste0("/home/lee/Dropbox/research/midas/", geoID, "/")
    outputFolder <- paste0("/home/lee/Dropbox/research/midas/outputs/", geoID, "/")
    if (!file.exists(outputFolder)) {
      dir.create(outputFolder)
    }
  } else {
    # Based on the Country Code, check to see if the 
    # outputs already exist. If they do, recursively remove
    # the directory and set up an empty one as the output folder. 
    # If not, just create the directory. 
    us_filepath <- "/data/shared_group_data/syneco/outputs/west/north_america/united_states/"
    if (file.exists(paste0(us_filepath, countryCode, "/"))) {
      unlink(paste0(us_filepath, countryCode, "/"), recursive = T)
      dir.create(paste0(us_filepath, countryCode, "/"))
      outputFolder<- paste0(us_filepath, countryCode, "/")
    } else {
      dir.create(paste0(us_filepath, countryCode, "/"))
      outputFolder<- paste0(us_filepath, countryCode, "/")
    }
  }
  
  countryParams<- list(countryCode=countryCode, year = year, geographyFileName=geographyFileName, 
                       shapeFileType=shapeFileType, PUMScountry=PUMScountry, hasPUMS=hasPUMS, 
                       addExtraVars=addExtraVars, shapefile=shapefile, PUMS=PUMS, hhAve=hhAve, 
                       table=table, numDecLatLong= numDecLatLong, varsToKeepH= varsToKeepH, 
                       varsToKeepP= varsToKeepP, seed=seed, countsGeoNames=countsGeoNames, 
                       geohiveLevel = geohiveLevel, PUMS.h=PUMS.h, PUMS.p=PUMS.p, dataFolder=dataFolder,   
                       outputFolder = outputFolder)
  return(countryParams)
}
