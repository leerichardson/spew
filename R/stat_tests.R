#' Perform chi square test on tract
#' 
#' @param regionID 11 digit ID for tract, 5 for county, 2 for state.  The FIPS number
#' @param type either "p" or "hh" for person or household output, respectively
#' @param output  SPEW synthetic ecosystem (data frame)
#' @param PUMS Public Use Micro Sample to compare joint distributions to
#' @param variables variable(s) to test.  No more than pairs
#' @return list of p value for region and original chi square value
#' @details  to come
stat_test_us_pums <- function(regionID, type = "p", level = "tract", output, PUMS,  variables = c("RAC1P", "SEX")){
    ## TODO:  what happpens when we don't have all categories??
    stopifnot(sum(variables %in% colnames(output)) == length(variables))
    print(variables)
    synth_tab <- table(output[, variables])
    pums_tab <- table(PUMS[, variables])
    stopifnot(length(synth_tab) == length(pums_tab))
    p <- pums_tab / sum(pums_tab)
    chi <- chisq.test(x = synth_tab, p = p)
    out_list <- list(regionID = regionID, obs = synth_tab, p = p, chi_sq = chi, variables = variables)
    return(out_list)
}

#' Outer function to perform chi square tests
#' 
#' @param regionID 11 digit ID for tract, 5 for county, 2 for state.  The FIPS number
#' @param type either "p" or "hh" for person or household output, respectively
#' @param output_folder  output_folder (top level)
#' @param PUMS_folder Public Use Micro Sample folder
#' @param variables variable(s) to test.  Must be for a single test
#' @return list with region ID, variable comparisons, and chi square results
#' @details  to come
stat_test_us_pums_outer <- function(regionID, type = "p", level = "tract", output_folder,
                                    PUMS_folder, variables = c("RAC1P", "SEX", "AGEP")){
    ## Load in the output and PUMS
    ## THE OUTPUT
    type_char<- ifelse(type == "p", "people", "household")
    output <- read.csv(file.path(output_folder, paste0(type_char, "_", regionID, ".csv")))
    ## THE PUMS
    puma_id <- unique(output$puma_id)[1]
    filepaths <- list.files(PUMS_folder)
    ind <- which(substr(filepaths, 5, 5) == type)[1] ## THIS IS FRAGILE!
    PUMS_full <- read.csv(file.path(PUMS_folder, filepaths[ind]))
    PUMS <- PUMS_full[PUMS_full$PUMA== puma_id,]

    ## Loop through single vars and pairs of vars
    nPairs <- choose(length(variables), 2)
    nTests <- nPairs + length(variables)
    ll <- vector(mode = "list", length = nTests)
    kk <- 1
    ## TODO:  make this more efficient
    for(jj in 1:length(variables)){
        var <- variables[jj]
        test <- stat_test_us_pums(regionID = regionID, type = type, level = level,
                                  output = output, PUMS = PUMS,
                                  variables = var)
        ll[[kk]] <- test
        kk <- kk + 1
    }
    ## Pairs of vars
    var_pairs <- combn(variables, 2)
    for (mm in 1:ncol(var_pairs)){
        var <- var_pairs[, mm]
        test <- stat_test_us_pums(regionID = regionID, type = type, level = level,
                                  output = output, PUMS = PUMS,
                                  variables = var)
        ll[[kk]] <- test
        kk <- kk + 1
    }
    return(ll)
}


### TESTING
library(devtools)
setwd("~/spew")
#load_all()

regionID <- "46113940800"
variables <- c("RAC1P", "SEX")
alpha <- .05
output <- read.csv("~/Desktop/46/output_200/eco/people_46113940800.csv")
##
output_folder <- "~/Desktop/46/output_200/eco/"
PUMS_folder <- "~/Desktop/46/input/pums/2013/"
##
puma_id <- unique(output$puma_id)[1]
PUMS_full <- read.csv(file.path("~/Desktop/46/input/pums/2013/ss13psd.csv"))
PUMS <- PUMS_full[PUMS_full$PUMA== puma_id,]

stat_test_us_pums(regionID, type = "p", level = "tract", output, PUMS, variables = variables)

tests <- stat_test_us_pums_outer(regionID, type = "p", level = "tract", output_folder,
                                 PUMS_folder, variables = c("RAC1P", "SEX"))

chi_list <- lapply(tests, function(ll) ll$chi_sq)
pvals <- lapply(chi_list, function(ll) ll$p.value)



