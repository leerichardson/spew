# Shannon Gallagher MIDAS CMU Stat May 13, 2015 functions for reading in
# 'other' data

# FUNCTION: Read in school data INPUTS: stno (5 char state county FPS);
# dataFolder (char); type (char) OUTPUTS: data frame REQUIREMENTS:
readSchoolData <- function(stno, dataFolder, type = "public") {
  if (type == "public") {
    filename <- paste0(dataFolder, "schools/ELSI_2011_public_f.csv")
    schoolsDF <- read.csv(filename, as.is = TRUE)
    schoolsDF$State.Code = gsub("=", "", schoolsDF$State.Code)
    schoolsDF <- schoolsDF[as.integer(as.character(schoolsDF$State.Code)) == 
                             as.integer(stno), ]
    noLocInds <- is.na(schoolsDF$Long)
    schoolsDF <- schoolsDF[!noLocInds, ]
    return(schoolsDF)
  } else if (type == "private") {
    filename <- paste0(dataFolder, "schools/ELSI_2010_private_f.csv")
    schoolsDF <- read.csv(filename, header = T, as.is = TRUE)
    schoolsDF$State.Code = gsub("=", "", schoolsDF$State.Code)
    schoolsDF <- schoolsDF[as.integer(as.character(schoolsDF$State.Code)) == 
                             as.integer(stno), ]
    return(schoolsDF)
  }
  stop("Choose either public or private schools.")
}

# FUNCTION: Read in workplace data INPUTS: stno (5 char state county
# FPS); dataFolder (char); OUTPUTS: data frame REQUIREMENTS:
readWorkplaceData <- function(stno, dataFolder) {
  stno <- as.integer(stno)
  filename <- paste0(dataFolder, "workplaces/us.workplaces_2009_", stno, 
                     ".csv")
  workplaceDF <- read.csv(filename)
  return(workplaceDF)
} 
