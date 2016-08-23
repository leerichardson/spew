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
    #browser()
    stopifnot(sum(variables %in% colnames(output)) == length(variables))
    PUMS_f <-  makeFactors(PUMS, PUMS, variables)
    output_f <- makeFactors(output, PUMS, variables)
    stopifnot(nrow(output_f) == nrow(output))
    synth_tab <- table(output_f[, variables])
    synth_p <- synth_tab / sum(synth_tab)
    ## Correct zero marginals
    pums_tab <- table(PUMS_f[, variables])
    stopifnot(length(synth_tab) == length(pums_tab))
    p <- pums_tab / sum(pums_tab)
    ## n <- sum(synth_tab)
    ## my own test
    ## e <- n * p
    ## chisq <- sum((e - synth_tab)^2/e)
    ## if (length(variables) == 1){
    ##     dof <- length(synth_tab) - 1
    ## } else{
    ##     dof <- (nrow(synth_tab) - 1) * (ncol(synth_tab) - 1)
    ## }
    ## pval <- 1 - pchisq(chisq, df = dof)
    chi <- chisq.test(synth_tab, p = p)
    nObs <- nrow(output)
    puma_id <- output$puma_id[1]
    print(regionID)
    print(nrow(output))
    out_list <- list(regionID = regionID, obs = synth_tab, p = p, type = type,
                     chi_sq = chi, variables = variables, nObs = nObs, puma_id = puma_id)
    return(out_list)
}

#' Turn variables into factors, matching level of PUMS
#'
#' @param in_df df
#' @param my_levels what levels to use
#' @param variables the variables to change
#' @return out_df of factors
makeFactors <- function(in_df, my_levels, variables){
  #  browser()
    for(var in variables){
        ## If the unique values are too many (i.e. continuous) then cut into 10 categories
        ## This value is completely arbitrary
        if ( length(unique(my_levels[, var])) > 20){
            print(paste("cutting", var))
            brks <- quantile(my_levels[, var], 1:10/10, na.rm = TRUE)
            cur_var <- cut(my_levels[,var], breaks=brks)
            in_df[, var] <- cut(in_df[, var], breaks = brks)
        } else{
            ## Otherwise factor the variables in the usual way
            cur_var <- factor(my_levels[, var])
            cur_levels <- levels(cur_var)
            in_df[, var] <- factor(in_df[, var], levels = cur_levels)
        }
    }
    return(in_df)
}



#' Outer function to perform chi square tests
#' 
#' @param regionID 11 digit ID for tract, 5 for county, 2 for state.  The FIPS number
#' @param type either "p" or "hh" for person or household output, respectively
#' @param output_folder  output_folder (top level)
#' @param PUMS_full Public Use Micro Sample (data frame) for whole state
#' @param variables variable(s) to test.  Must be for a single test
#' @return list with region ID, variable comparisons, and chi square results
#' @details  to come
stat_test_us_pums_outer <- function(regionID, type = "p", level = "tract", output_folder,
                                    PUMS_full, variables = c("RAC1P", "SEX", "AGEP"),
                                    puma_id = puma_id){
    ## Load in the output and PUMS
    ## THE OUTPUT
   # browser()
    type_char <- ifelse(type == "p", "people", "household")
    if(type == "b"){
        p <- read.csv(file.path(output_folder, paste0("people_", regionID, ".csv")))
        hh <-  read.csv(file.path(output_folder, paste0( "household_", regionID, ".csv")))
        output <- join(hh, p, by = "SERIALNO", match = "first")
    } else{
        output <- read.csv(file.path(output_folder, paste0(type_char, "_", regionID, ".csv")))
    }
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
    if (length(variables) > 1){
        var_pairs <- combn(variables, 2)
        for (mm in 1:ncol(var_pairs)){
            var <- var_pairs[, mm]
            test <- stat_test_us_pums(regionID = regionID, type = type, level = level,
                                      output = output, PUMS = PUMS,
                                      variables = var)
            ll[[kk]] <- test
            kk <- kk + 1
        }
    }
    return(ll)
}


#' Loop over regions in a state folder to perform chi square tests
#' 
#' @param output_folder  output_folder (top level)
#' @param PUMS_folder folder to  Public Use Micro Sample (data frame) for whole state
#' @param household_vars string of household variables to test
#' @param people_vars string of people vars to test
#' @param householder_vars string of head of householder vars to test
#' @return list with region ID, variable comparisons, and chi square results
#' @details  to come
test_features <- function(output_folder, PUMS_folder, household_vars = c("HINCP", "NP"),
                          people_vars = c("RAC1P", "SEX"),
                          householder_vars = c("RAC1P", "AGEP", "HINCP", "NP")){
    ## load in the PUMS
    #browser()
    filepaths <- list.files(PUMS_folder)
    hh_ind <- which(substr(filepaths, 5, 5) == "h")[1] ## THIS IS FRAGILE!
    p_ind <- which(substr(filepaths, 5, 5) == "p")[1]
    hh_PUMS <- read.csv(file.path(PUMS_folder, filepaths[hh_ind]))
    p_PUMS <- read.csv(file.path(PUMS_folder, filepaths[p_ind]))
    b_PUMS <- join(hh_PUMS, p_PUMS, by = "SERIALNO", match = "first")

    ## Loop over the output folders
    output_paths <- list.files(output_folder)
    output_paths <- output_paths[grepl("output_", output_paths)]
    ll <- vector(mode = "list")
    for (output_path in output_paths){
        new_path <- file.path(output_folder, output_path, "eco")
        region_files <- list.files(new_path)
        regions <- unique(gsub("[^0-9]", "", region_files))
        puma_id <- gsub("[^0-9]", "", output_path)
        ## Loop over the regions
        for (rr in 1:length(regions)){
            region <- regions[rr]
            print(paste(puma_id, region))
          #  print(region)
            ## Household
            hh_list <- NULL
            if(length(household_vars) > 0){
                hh_list <- stat_test_us_pums_outer(regionID = region, type = "hh",
                                                   level = "tract", output_folder = new_path,
                                                   PUMS = hh_PUMS, variables = household_vars,
                                                   puma_id = puma_id)
            }
            ## Person
            p_list <- NULL
            if(length(people_vars) > 0){ 
                p_list <- stat_test_us_pums_outer(regionID = region, type = "p",
                                                  level = "tract", output_folder = new_path,
                                                  PUMS = p_PUMS, variables = people_vars,
                                                  puma_id = puma_id)
            }
            ## Both
            b_list <- NULL
            if(length(householder_vars) >0 ){
                b_list <- stat_test_us_pums_outer(regionID = region, type = "b",
                                                  level = "tract", output_folder = new_path,
                                                  PUMS = b_PUMS, variables = householder_vars,
                                                  puma_id = puma_id)
            }
            ## Combining together into one long list
            new_list <- c(hh_list, p_list, b_list)
            ll <- c(ll, new_list)
        }
       
    }
    print("Number of regions")
    print(length(ll))
    return(ll)
}

#' Turn the output from test_features into a df
#'
#' @param test_list output from test_features
#' @return a data frame with region id, variables (separated by a dash), chi. sq. value, dof, and p value
makeStatDF <- function(features_list){
    regionID <- sapply(features_list, function(ll) ll$regionID)
    vars <- sapply(features_list, function(ll) paste(ll$variables, collapse = "-"))
    chisq <- sapply(features_list, function(ll) ll$chi_sq$statistic)
    dof <-  sapply(features_list, function(ll) ll$chi_sq$parameter)
    p.value <- sapply(features_list, function(ll) ll$chi_sq$p.value)
    nObs <- sapply(features_list, function(ll) ll$nObs)
    type <- sapply(features_list, function(ll) ll$type)
    puma_id <- sapply(features_list, function(ll) ll$puma_id)
    stat_df <- data.frame(regionID = regionID, vars = vars, chisq = chisq,
                          dof = dof, pval = p.value, nObs = nObs, type = type, puma_id = puma_id)
    return(stat_df)
}


#' Test schools/workplaces
#'
#' @param syneco_folder string
#' @param input_folder string
#' @param admin_level integer TBD
#' @param syneco_vars variables to keep
#' @param env_type "sch", "wpl", "both" currently
#' @return list with two data frames.  One from the syneco, other with input data from schools, workplaces or other
makeEnvironmentsDFs <- function(syneco_folder, input_folder, admin_level = 0,
                               syneco_vars = c("school_id", "workplace_id", "SCH",
                                               "SCHG", "AGEP", "ESR"),
                               env_type = "both"){
    stopifnot(env_type %in% c("sch", "wpl", "both"))
    ## Read in synecos
    syneco_df <- readSynecos(syneco_folder, type = "people", syneco_vars)
    ## Read in input data
    if(env_type == "both"){

    }
}

#' Read and subset synecos
#'
#' @param syneco_folder string
#' @param type "people" or "household"
#' @param syneco_vars vars to subset along with 'place_id'
readSynecos <- function(syneco_folder, type = "people",
                        syneco_vars = c("school_id", "workplace_id", "SCH",
                                               "SCHG", "AGEP", "ESR")){
    stopifnot(type %in% c("people", "household"))
    files <- list.files(syneco_folder, recursive = TRUE)
    files <- files[grepl(type, files)]
    nums <- gsub("[^0-9]", "",  basename(files))
    people_files <- files[ nchar(nums) >= 11]
    full_paths <- file.path(syneco_folder, people_files)
    ll <- vector(mode = "list", length(full_paths))
    for (ii in 1:length(full_paths)){
        if(ii %% 50 == 0) print(ii)
        df <- read.csv(full_paths[ii])
        df <- subset(df, select = c("place_id", syneco_vars))
        ll[[ii]] <- df
    }
    df <- do.call('rbind', dfs_list)
    return(df)
}
