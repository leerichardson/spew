# 2015 January 16 Shannon Gallagher, Beka Steorts, Lee Richardson The Run
# File Use this file to create synthetic data!

# Obtain the country name from the Rscript command line argument. If
# running from source, we can still
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  library(methods)
  countryName <- args[1]
} else {
  countryName <- "canada"
}
geoID <- countryName  # US only

# Set the parameters to be used for this run through of the syntheic
# population generator. 
user <- "v2_user"
version <- "v3"
censusYear <- 2013  # dummy variable currently  
geoLevel <- 2
isUSA <- FALSE
samplingMethod <- "unif"
inputFolder <- "/data/shared_group_data/syneco/v3/"

# Switch the input folder on my local computers
if (Sys.getenv("USER") == "lee") {
  inputFolder <- "/home/lee/Dropbox/research/midas/v3/"
}

# Call a single country
source(paste0(inputFolder, "mainRun/main_v3.R"))
ptm <- proc.time()
mainRun(countryName, censusYear, geoLevel, version, user, inputFolder, isUSA, 
        geoID, samplingMethod)
print(proc.time() - ptm) 
