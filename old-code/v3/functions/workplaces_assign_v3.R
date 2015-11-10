# Shannon Gallagher April 26, 2015 Workplace Assigments

# FUNCTION: split a DF by the state numbera nd write separate dfs INPUTS:
# stno (char vec); df( df to split); stVec vec of st numbers; path
# (char); filename(char) OUTPUTS: LOGICAL REQUIREMENTS:
splitDFByState <- function(stno, df, stVec, path, filename) {
  inds <- which(as.character(stVec) == as.character(stno))
  new.df <- df[inds, ]
  new.fn <- paste0(filename, "_", stno, ".csv")
  write.csv(new.df, paste0(path, new.fn), row.names = FALSE)
  return(length(inds))
}

# FUNCTION: getWorkplace, high level assigner of workplaces for synthPUMS
# people INPUTS: synth Inds (int vec); POWPUMA (int vec); workplaceDF
# (df); lookUpTable (df); stno (int); stco (int) OUTPUTS: vector of
# workplace assigments REQUIREMENTS: assign.Workplaces

getWorkplace <- function(synthInds, POWPUMA, workplaceDF, lookUpTable, stno, 
                         stco) {
  # stcotr<- as.character(workplaceDF$stcot stcotr.df<- sapply(stcotr,
  # breakUpFPS)
  stco.vec <- paste0(workplaceDF$st, workplaceDF$co)
  subsettedWorkplaces <- workplaceDF[as.integer(stco.vec) == as.integer(stco), 
                                     ]
  workplaceIDs <- assign.Workplaces(synthInds, POWPUMA, subsettedWorkplaces, 
                                    lookUpTable, stno, stco)
}


# FUNCTION: assign workplace using the POWPUMA variable INPUTS: synthID
# (int); POWPUMA( integer); subsettedWorkplaces (df); lookUpTable (df);
# stno(int); stco(numeric vector) OUTPUTS: vector of the workplace IDs
# ATTN: need to make more general and sample multiple PUMAs at a time,
# get plyr out also check if there is another variable for employment
# works for indices in the same tract FIX: Work for a tract of synthetic
# indices at a time so subsetttedWorkplaces consists only of the
# workplaces in the proper county
assign.Workplaces <- function(synthInds, POWPUMA, subsettedWorkplaces, lookUpTable, 
                              stno, stco) {
  notNAinds <- which(!is.na(POWPUMA[synthInds]))
  workplaceIDs <- numeric(length(synthInds))
  workplaceIDs[-notNAinds] <- NA
  workplaceIDs[notNAinds] <- assignWorkplaces.inner(synthInds, POWPUMA, 
                                                    subsettedWorkplaces, lookUpTable, stno, stco)
  return(workplaceIDs)
}



assignWorkplaces.inner <- function(synthInds, POWPUMA, subsettedWorkplaces, 
                                   lookUpTable, stno, stco) {
  # Do this step in the preprocessing
  goodCos <- lookUpTable[lookUpTable$st == as.integer(stno), ]
  goodCos <- goodCos[goodCos$puma %in% POWPUMA[synthInds], "stco"]
  good.inds <- which(stco %in% as.integer(goodCos))
  if (length(good.inds) < 1) {
    samp.ind <- sample(1:nrow(subsettedWorkplaces), length(synthInds), 
                       prob = (subsettedWorkplaces$employees/sum(subsettedWorkplaces$employees)), 
                       replace = TRUE)
    return(as.character(subsettedWorkplaces[samp.ind, 1]))
  } else {
    samp.ind <- sample(good.inds, size = length(synthInds), 
                       prob = (subsettedWorkplaces$employees[good.inds]/sum(subsettedWorkplaces$employees[good.inds])), 
                       replace = TRUE)
    return(as.character(subsettedWorkplaces[samp.ind, 1]))
  }
  
} 
