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
        p <- p[p$RELP == 0,]
        hh <-  read.csv(file.path(output_folder, paste0( "household_", regionID, ".csv")))
        output <- join(hh, p, by = "SERIALNO", type = "left")
        output <- output[!duplicated(output$SYNTHETIC_HID),]
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



#' Turn variables into factors, matching level of PUMS
#'
#' @param in_df df
#' @param my_levels what levels to use
#' @param variables the variables to change
#' @return out_df of factors
makeFactorsMarg <- function(in_df, marginals, variables){
 #  browser()
   for(var in variables){
        ## Cut according to marginals${var}$lookup
        levels <- marginals[[var]]$lookup$marg_names
        new_vals <- rep("k", nrow(in_df))
        for (ii in 1:length(levels)){
            lower <- marginals[[var]]$lookup$lower[ii]
            upper <- marginals[[var]]$lookup$upper[ii]
           # print(c(lower, upper))
            nm <- marginals[[var]]$lookup$marg_names[ii]
            new_vals<- ifelse( ( in_df[, var] >= lower) & ( in_df[,var] <= upper ), nm, new_vals)
        }
       new_fac <- factor(new_vals, levels = levels)
       in_df[,var] <- new_fac
    }
    return(in_df)
}


#' Perform chi square test on tract for marginals totals
#' 
#' @param regionID 11 digit ID for tract, 5 for county, 2 for state.  The FIPS number
#' @param type either "p" or "hh" for person or household output, respectively
#' @param output  SPEW synthetic ecosystem (data frame)
#' @param PUMS Public Use Micro Sample to compare joint distributions to
#' @param variables variable(s) to test.  No more than pairs
#' @return list of p value for region and original chi square value
#' @details  to come
stat_test_us_marg <- function(regionID, type = "p", level = "tract", output, marginals,  variables = c("RAC1P")){
    #browser()
    stopifnot(sum(variables %in% colnames(output)) == length(variables))
    output_f <- align_pums(output, marginals)
    stopifnot(nrow(output_f) == nrow(output))
    synth_tab <- table(output_f[, paste0(variables, "_marg")])
    synth_p <- synth_tab / sum(synth_tab)
    ## Correct zero marginals
    row_ind <- which( marginals[[variables]]$df$place_id == regionID)
    stopifnot(output$place_id[1] == regionID)
    stopifnot(length(row_ind) == 1)
    marg_tab <- marginals[[variables]]$df[row_ind, -1]
    stopifnot(identical(names(marg_tab), names(synth_tab)))
    p <- unlist(marg_tab) / sum(unlist(marg_tab))
    p_test <- p
    synth_test <- synth_tab
    chi <- chisq.test(synth_test, p = p_test)
    nObs <- nrow(output)
    puma_id <- output$puma_id[1]
    print(regionID)
    #print(nrow(output))
    out_list <- list(regionID = regionID, obs = synth_tab, p = p, type = type,
                     chi_sq = chi, variables = variables, nObs = nObs, puma_id = puma_id)
    return(out_list)
}

#' Test Marginal totals to synth pop using chi square
#'
#' @param regionID 11 digit string
#' @param type "b"
#' @param level "tract"
#' @param output_folder path
#' @param marginals output from readMarginals
#' @param variables in c("RAC1P", "AGEP", "HINCP", "NP")
stat_test_us_marg_outer <- function(regionID, type = "b", level = "tract", output_folder,
                                    marginals, variables = c("RAC1P", "AGEP", "HINCP", "NP"), puma_id = puma_id){
##    browser()
    ## Load in the output (synthetic agents)
    stopifnot(type == "b")
    if(type == "b"){
        p <- read.csv(file.path(output_folder, paste0("people_", regionID, ".csv")))
        p <- p[p$RELP == 0,]
        hh <-  read.csv(file.path(output_folder, paste0( "household_", regionID, ".csv")))
        output <- join(hh, p, by = "SERIALNO", type = "left")
        output <- output[!duplicated(output$SYNTHETIC_HID),]
    }
    ## Loop through single vars 
    ll <- vector(mode = "list", length = length(variables))
    kk <- 1
    ## TODO:  make this more efficient
    for(jj in 1:length(variables)){
        var <- variables[jj]
        test <- stat_test_us_marg(regionID = regionID, type = type, level = level,
                                  output = output, marginals = marginals,
                                  variables = var)
        ll[[kk]] <- test
        kk <- kk + 1
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

## Marginals

#' Read in marginal objects from the ACS SF
#'
#' @param cur_co current county character (3 digits)
#' @param marginal_folder path to marginals
#' @param householder_vars currently in c("NP", "HINCP", "RAC1P", "AGEP")
#' @return list with each entry as list with data frame for the householder_var along with lookup and type
readMarginals <- function(cur_co, marginal_folder, householder_vars){
    stopifnot(sum(householder_vars %in% c("NP", "HINCP", "RAC1P", "AGEP")) == length(householder_vars))
    st <- substr(list.files(marginal_folder)[1], 1, 2)
    marginals <- readRDS(file.path(marginal_folder, paste0(st, "_marginals.RDS")))
    return(marginals)
}



## JOINT

#' Read in joint objects from spew generation of IPF tables
#'
#' @param cur_co current county character (3 digits)
#' @param joint_folder path to marginals
#' @param householder_vars currently in c("NP", "HINCP", "RAC1P", "AGEP")
#' @return list with each entry as list with data frame for the householder_var along with lookup and type
readJoint <- function(regionID, joint_folder, householder_vars){
    stopifnot(sum(householder_vars %in% c("NP", "HINCP", "RAC1P", "AGEP")) == length(householder_vars))
    joint_table<- readRDS(file.path(joint_folder,  paste0(regionID, "_table.RDS")))
    new_names <- gsub("_marg", "", names(dimnames(joint_table)))
    names(dimnames(joint_table)) <- new_names
    return(joint_table)
}





#' Perform chi square test on tract for joint totals
#' 
#' @param regionID 11 digit ID for tract, 5 for county, 2 for state.  The FIPS number
#' @param type either "p" or "hh" for person or household output, respectively
#' @param output  SPEW synthetic ecosystem (data frame)
#' @param joint_folder folder to IPF tables saved from generation
#' @param variables variable(s) to test.  No more than pairs
#' @param marginals marginal folder to get factor names
#' @return list of p value for region and original chi square value
#' @details  to come
stat_test_us_joint <- function(regionID, type = "p", level = "tract", output, joint_table,  variables = c("RAC1P"), marginals){
                                        #browser()
    ## Get the marginal table for the 2 dimensions in the right order
    dim_ind1 <- which(names(dimnames(joint_table)) == variables[1])
    dim_ind2 <- which(names(dimnames(joint_table)) == variables[2])
    dim_inds <- c(dim_ind1, dim_ind2)
    joint_tab <- margin.table(joint_table, dim_inds)
    stopifnot(sum(variables %in% colnames(output)) == length(variables))
    ## Put the output into the proper factors
    output_f <- align_pums(output, marginals)
    stopifnot(nrow(output_f) == nrow(output))
    synth_tab <- table(output_f[, paste0(variables, "_marg")])
    synth_p <- synth_tab / sum(synth_tab)
    p <- joint_tab / sum(joint_tab)
    ## Compare the output
    chi <- chisq.test(synth_tab, p = p)#, simulate.p.value = TRUE)
    nObs <- nrow(output)
    puma_id <- output$puma_id[1]
    print(regionID)
    print(nrow(output))
    out_list <- list(regionID = regionID, obs = synth_tab, p = p, type = type,
                     chi_sq = chi, variables = variables, nObs = nObs, puma_id = puma_id)
    return(out_list)
}



#' Test joint totals to synth pop using chi square
#'
#' @param regionID 11 digit string
#' @param type "b"
#' @param level "tract"
#' @param output_folder path
#' @param joint_folder joint folder to ipf table from syneco generation
#' @param variables in c("RAC1P", "AGEP", "HINCP", "NP")
stat_test_us_joint_outer <- function(regionID, type = "b", level = "tract", output_folder,
                                   joint_folder, variables = c("RAC1P", "AGEP", "HINCP", "NP"), puma_id = puma_id, marginals){
    ## Load in the output (synthetic agents)
    stopifnot(type == "b")
    if(type == "b"){
        p <- read.csv(file.path(output_folder, paste0("people_", regionID, ".csv")))
        p <- p[p$RELP == 0,]
        hh <-  read.csv(file.path(output_folder, paste0( "household_", regionID, ".csv")))
        output <- join(hh, p, by = "SERIALNO", type = "left")
        output <- output[!duplicated(output$SYNTHETIC_HID),]
    }
    ## Load in the joint table
    joint_table <- readJoint(regionID, joint_folder, variables)
    ## Loop through single vars 
    ll <- vector(mode = "list", length = length(variables))
    kk <- 1
    cmbxns <- combn(variables, 2)
    nTests <- ncol(cmbxns)

    ## TODO:  make this more efficient
    for(jj in 1:ncol(cmbxns)){
        var <- cmbxns[ ,jj]
        test <- stat_test_us_joint(regionID = regionID, type = type, level = level,
                                  output = output, joint_table = joint_table,
                                  variables = var, marginals = marginals)
        ll[[kk]] <- test
        kk <- kk + 1
    }
    return(ll)
}



#' Loop over regions in a state folder to perform chi square tests
#' 
#' @param output_folder  output_folder (top level)
#' @param marginal_folder folder to  marginal SF
#' @param household_vars string of household variables to test
#' @param people_vars string of people vars to test
#' @param householder_vars string of head of householder vars to test
#' @return list with region ID, variable comparisons, and chi square results
#' @details  to come
test_features_marg<- function(output_folder, marginal_folder, householder_vars = NULL){
    ## Read marginal variables
    cur_co <- 4
    marginals <- readMarginals(cur_co, marginal_folder, householder_vars)
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
            ## Both
            b_list <- NULL
            if(length(householder_vars) >0 ){
                b_list <- stat_test_us_marg_outer(regionID = region, type = "b",
                                                  level = "tract", output_folder = new_path,
                                                  marginals = marginals, variables = householder_vars,
                                                  puma_id = puma_id)
            }
            ## Combining together into one long list
            new_list <-  b_list
            ll <- c(ll, new_list)
        }
       
    }
    print("Number of regions")
    print(length(ll))
    return(ll)
}



######### JOINT

#' Loop over regions in a state folder to perform chi square tests
#' 
#' @param output_folder  output_folder (top level)
#' @param marginal_folder folder to joint SF
#' @param household_vars string of household variables to test
#' @param people_vars string of people vars to test
#' @param householder_vars string of head of householder vars to test
#' @return list with region ID, variable comparisons, and chi square results
#' @details  to come
test_features_joint<- function(output_folder, joint_folder, householder_vars = NULL, marginal_folder){
    ## Read marginal variables
    cur_co <- 4
    marginals <- readMarginals(cur_co, marginal_folder, householder_vars)
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
            ## Both
            b_list <- NULL
            if(length(householder_vars) >0 ){
                b_list <- stat_test_us_joint_outer(regionID = region, type = "b",
                                                  level = "tract", output_folder = new_path,
                                                  joint_folder = joint_folder, variables = householder_vars,
                                                  puma_id = puma_id, marginals)
            }
            ## Combining together into one long list
            new_list <-  b_list
            ll <- c(ll, new_list)
        }
       
    }
    print("Number of regions")
    print(length(ll))
    return(ll)
}


################################################




#' Test schools/workplaces
#'
#' @param syneco_folder string
#' @param input_folder string
#' @param admin_level integer TBD
#' @param syneco_vars variables to keep
#' @param env_type "sch", "wpl", "both" currently
#' @return list of environmental data frames.  One from the syneco (people), others with input data from schools, workplaces or other
makeEnvironmentsDFs <- function(syneco_folder, input_folder, admin_level = 0,
                               syneco_vars = c("school_id", "workplace_id", "SCH",
                                               "SCHG", "AGEP", "ESR"),
                               env_type = "both"){
    stopifnot(env_type %in% c("sch", "wpl", "both"))
    
    ## Read in synecos
    syneco_df <- readSynecos(syneco_folder, type = "people", syneco_vars)
    
    ## Read in input data
    school_df <- NULL
    wpl_df <- NULL
    if(env_type == "both"){
        school_list <- readEnvUS(input_folder, type = "sch")
        wpl_df <- readEnvUS(input_folder, type = "wpl")
    } else if (env_type == "sch"){
        school_df <- readEnvUS(input_folder, type = "sch")
    } else if (env_type == "wpl"){
        wpl_df <- readEnvUS(input_folder, type = "wpl")
    }
    return(list(syneco_df = syneco_df, school_pub_df = school_list[[2]], school_priv_df = school_list[[1]], wpl_df = wpl_df))
}


#' Read input data for environments for US
#'
#' @param input_folder filepath
#' @param type either "sch" or "wpl" currently
#' @ return df of schools or workplaces
readEnvUS <- function(input_folder, type = "sch"){
    stopifnot(type %in% c("sch", "wpl"))
    if (type == "sch"){
        full_path <- file.path(input_folder, "schools", "2013")
        files <- list.files(full_path)
        env <- lapply(file.path(full_path, files), read.csv) # WARNING:  order should be private, public
    } else if (type == "wpl"){
        full_path <- file.path(input_folder, "workplaces")
        files <- list.files(full_path)
        env <- read.csv(file.path(full_path, files))
    }
    return(env)
}


#' Read and subset synecos
#'
#' @param syneco_folder string
#' @param type "people" or "household"
#' @param syneco_vars vars to subset along with 'place_id'
#' @return list or data frame of input data
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
        df <- read.csv(full_paths[ii], stringsAsFactors = FALSE)
        df <- subset(df, select = c("place_id", "longitude", "latitude", syneco_vars))
        ll[[ii]] <- df
       # print(colnames(df))
    }
    df <- do.call('rbind', ll)
    return(df)
}

## Test the marginals
























#######################################################
############################# Schools and workplaces
####################################################
###################################################


#' Assess the environment use of our synthetic ecosystems
#'
#' @param syneco_list output from makeEnvironmentsDFs
#' @param type "wpl" "sch" or "both" currently
#' @return
assessSynecosUS <- function(syneco_list, type = "both"){
    stopifnot(env_type %in% c("sch", "wpl", "both"))
    wpl_a <- NULL
    sch_a <- NULL
    if (type == "both"){
        sch_a <- assessSchUS(agents = syneco_list$syneco_df, schools_pub = syneco_list$school_pub_df,
                             schools_priv = syneco_list$school_priv_df)
        wpl_a <- assessWplUS(agents = syneco_list$syneco_df, wpl = wpl_df)
    } else if ( type == "sch"){
        sch_a <- assessSchUS(agents = syneco_list$syneco_df, schools_pub = syneco_list$school_pub_df,
                             schools_priv = syneco_list$school_priv_df)
    } else if (type == "wpl"){
        wpl_a <- assessWplUS(agents = syneco_list$syneco_df, wpl = wpl_df)
    }
    return(list(sch_a = sch_a, wpl_a = wpl_a))
}

#' Assess the schools of the us against the agents
#'
assessSchUS <- function(agents, schools_pub, schools_priv, distFun = haversine){
    ## For agents$SCH
    ##  1 is none, 2 is public, 3 is private
    nStudents <- length(agents$school_id[!is.na(agents$school_id)])
    print(paste("There are", nStudents, "students"))

    ## assess schools
    nbadA <- length(agents$school_id[!is.na(agents$school_id) & agents$SCH == 1])
    print(paste(nbadA, "people have been assigned to schools when they should not be in school"))
    
    nSynSchools <- length(unique(agents$school_id[!is.na(agents$school_id)]))
    nTotSchools <- length(unique(schools_pub$ID)) + length(unique(schools_priv$ID))
    print(paste(nSynSchools, " schools have been used"))
    print(paste(nSynSchools / nTotSchools * 100,
                "% of schools have been used" ))
    n <- 15
    nTotSchools10 <- length(unique(schools_pub$ID[schools_pub$Students > n])) +
        length(unique(schools_priv$ID[schools_priv$Students > n]))
    print(paste(nSynSchools / nTotSchools10 * 100,
                "% of schools have been used that have at least", n, "students in capacity" ))

    ## private schools
    nSynPriv <- length(sort(unique(agents$school_id[agents$SCH == 3 & !is.na(agents$school_id)])))
    nPriv <- length(unique(schools_priv$ID))
     print(paste("There are", nSynPriv, "private schools"))
    print(paste(nSynPriv / nPriv * 100, "% of private schools have been used"))
    ## all private schools are used

    ## public schools
    nSynPub <- length(sort(unique(agents$school_id[agents$SCH == 2  & !is.na(agents$school_id)])))
    nPub <- length(unique(schools_pub$ID))
    nSynPub / nPub
    print(paste("There are", nSynPub, "public schools"))
    print(paste(nSynPub / nPub *  100, "% of public schools have been used"))

    ## dist to school
    school_children <- agents[!is.na(agents$school_id),]
    pub_children <- school_children[ school_children$SCH == 2,]
    schools_pub$school_id<- schools_pub$ID
    pub_j <- join(pub_children, schools_pub, by = "school_id")
    my_dists <- apply(pub_j, 1, function(row){
        x1 <- as.numeric(row['longitude'])
        x2 <- as.numeric(row['Long'])
        y1 <- as.numeric(row['latitude'])
        y2 <- as.numeric(row['Lat'])
        dist <- distFun(x1, y1, x2, y2)
        return(dist)
        })
    print(summary(my_dists))
    #hist(my_dists)
    return(my_dists)
}

#' Assess the schools of the us against the agents
#'
assessWplUS <- function(agents, wpl){
   
}


## Distances

#' Calculates the geodesic distance between two points
#'
#' @param long1 longitude (degrees)
#' @param lat1 latitude (degrees)
#' @param long2 longitude (degrees)
#' @param lat2 latitude (degrees)
#' @return returns distance in miles
#' @details specified by radian latitude/longitude using the
#' Spherical Law of Cosines (slc)
spher_dists<- function(long1, lat1, long2, lat2) {
    R <- 3959# Earth mean radius [mi]
    long1 <- deg2rad(long1)
    lat1 <- deg2rad(lat1)
    long2 <- deg2rad(long2)
    lat2 <- deg2rad(lat2)
    d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
    return(d) # Distance in miles
}

#' Converts degrees to radians
#' 
#' @param degree (lat/long coordinate)
#' @return degree in radians
deg2rad <- function(deg) return(deg*pi/180)

#' Compare capacities of generated schools and actual schools
#'
#' @param agents synthetic people with school_id var
#' @param list of public schools df and private schools df with Students variable with capacity
#' @return percent filled school (possibly over 1)
compareCapacities <- function(agents, schools){
    ## synthetic children counts
    school_children <- agents[!is.na(agents$school_id),]
    synth_tab <- data.frame(table(school_children$school_id))
    colnames(synth_tab) <- c("ID", "Capacity")
    ## actual school capacities
    pub <- schools[[1]][, c("ID", "Students")]
    priv <- schools[[2]][, c("ID", "Students")]
    school_df <- rbind(pub, priv)
    df_j <- join(school_df, synth_tab, by = "ID")
    df_j$Capacity <- ifelse(is.na(df_j$Capacity), 0, df_j$Capacity)
    percents <- df_j$Capacity / df_j$Students * 100
    return(percents)
}
