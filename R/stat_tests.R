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
    PUMS_f <-  makeFactors(PUMS, PUMS, variables)
    output_f <- makeFactors(output, PUMS, variables)
    stopifnot(nrow(output_f) == nrow(output))
    #print(variables)
    synth_tab <- table(output_f[, variables])
    ## Correct zero marginals
    pums_tab <- table(PUMS_f[, variables])
    stopifnot(length(synth_tab) == length(pums_tab))
    p <- pums_tab / sum(pums_tab)
    chi <- chisq.test(x = synth_tab, p = p, simulate.p.value = FALSE)
    nObs <- nrow(output)
    print(regionID)
    print(nrow(output))
    out_list <- list(regionID = regionID, obs = synth_tab, p = p, type = type, chi_sq = chi, variables = variables, nObs = nObs)
    return(out_list)
}

#' Turn variables into factors, matching level of PUMS
#'
#' @param in_df df
#' @param my_levels what levels to use
#' @param variables the variables to change
#' @return out_df of factors
makeFactors <- function(in_df, my_levels, variables){
    for(var in variables){
        ## If the unique values are too many (i.e. continuous) then cut into 10 categories
        ## This value is completely arbitrary
        if (length(unique(my_levels[, var])) > 20){
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
                                    PUMS_full, variables = c("RAC1P", "SEX", "AGEP")){
    ## Load in the output and PUMS
    ## THE OUTPUT
    type_char<- ifelse(type == "p", "people", "household")
    if(type == "b"){
        hh <- read.csv(file.path(output_folder, paste0("people_", regionID, ".csv")))
        p <-  read.csv(file.path(output_folder, paste0( "household_", regionID, ".csv")))
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
        ## Loop over the regions
        for (rr in 1:length(regions)){
            region <- regions[rr]
          #  print(region)
            ## Household
            hh_list <- NULL
            if(length(household_vars) > 0){
                hh_list <- stat_test_us_pums_outer(regionID = region, type = "hh",
                                                   level = "tract", output_folder = new_path,
                                                   PUMS = hh_PUMS, variables = household_vars)
            }
            ## Person
            p_list <- NULL
            if(length(people_vars) > 0){ 
                p_list <- stat_test_us_pums_outer(regionID = region, type = "p",
                                                  level = "tract", output_folder = new_path,
                                                  PUMS = p_PUMS, variables = people_vars)
            }
            ## Both
            b_list <- NULL
            if(length(householder_vars) >0 ){
                b_list <- stat_test_us_pums_outer(regionID = region, type = "b",
                                                  level = "tract", output_folder = new_path,
                                                  PUMS = b_PUMS, variables = householder_vars)
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
    stat_df <- data.frame(regionID = regionID, vars = vars, chisq = chisq,
                          dof = dof, pval = p.value, nObs = nObs, type = type)
    return(stat_df)
}


## Lee must love this part

## ### TESTING
## library(devtools)
## library(plyr)
## setwd("~/spew")
## # load_all()

## regionID <- "46113940800"
## variables <- c("RAC1P", "SEX")
## alpha <- .05
## output <- read.csv("~/Desktop/46/output_200/eco/people_46113940800.csv")
## ##
## output_folder <- "~/Desktop/46/output_200/eco/"
## PUMS_folder <- "~/Desktop/46/input/pums/2013/"
## ##
## puma_id <- unique(output$puma_id)[1]
## PUMS_full <- read.csv(file.path("~/Desktop/46/input/pums/2013/ss13psd.csv"))
## PUMS <- PUMS_full[PUMS_full$PUMA== puma_id,]

## spew:::stat_test_us_pums(regionID, type = "p", level = "tract", output, PUMS, variables = variables)

## tests <- spew:::stat_test_us_pums_outer(regionID, type = "p", level = "tract", output_folder,
##                                  PUMS, variables = c("RAC1P", "SEX"))

## chi_list <- lapply(tests, function(ll) ll$chi_sq)
## pvals <- lapply(chi_list, function(ll) ll$p.value)


## output_folder <- "~/Desktop/46/"
## PUMS_folder <- "~/Desktop/46/input/pums/2013/"


## t <- proc.time()[3]
## features_list <- test_features(output_folder, PUMS_folder,
##                           household_vars = NULL, #c("NP", "HINCP"),
##                           people_vars = NULL, #c("SEX", "AGEP")
##                           householder_vars = c("RAC1P", "NP", "AGEP", "HINCP"))
## proc.time()[3] - t

## my_df <- makeStatDF(features_list)
## my_df$pval <- ifelse(is.nan(my_df$pval), 1, my_df$pval)
## alpha <- .05/ (nrow(my_df))
## alpha_df <- data.frame(alpha = alpha, lt = "alpha value")
## ## ggplot
## library(ggplot2)
## ggplot() + geom_boxplot(data = my_df, aes(factor(vars), log(pval) )) + geom_hline(data = alpha_df,
##                                                                                   aes(yintercept = log(alpha), linetype =lt,
##                                                                                       show.legend = TRUE), col = "red") +
##     coord_flip() + ggtitle(expression(atop("Pearson Chi-Square p-values", paste("Tracts in SD; ", alpha, " = .05, Adjusted for Multiple Comparisons") ))) +     labs(y = "log(p-value)", x = "Population Characteristic(s)", col = "# Agents")  +
##     scale_linetype_manual(name = "",values = 1,guide = "legend",  lab = expression(log(alpha))) + theme_light() +
##        theme(
##              axis.text.x = element_text(size = 12, family = "Palatino"),
##              axis.text.y= element_text(size = 12, family = "Palatino"),
##              axis.title.x= element_text(size = 16, family = "Palatino"),
##              axis.title.y= element_text(size = 16, family = "Palatino"),
##              plot.title = element_text(size = 20, family = "Palatino"),
##              legend.title = element_text(size = 16, family = "Palatino"),
##              legend.text = element_text(family = "Palatino")
##        )  
## #ggsave("~/Desktop/sd_pvals.pdf")

## ## Splitting by tract
## ## Taking a sample of 20
## regions <- sample(unique(my_df$regionID), 20)
## new_df <- my_df[as.character(my_df$regionID) %in% as.character(regions),]
## ggplot() + geom_boxplot(data = new_df, aes(factor(regionID), log(pval), col = nObs)) + geom_hline(data = alpha_df,
##                                                                                   aes(yintercept = log(alpha), linetype =lt,
##                                                                                       show.legend = TRUE), col = "red")  + 
##     coord_flip() + ggtitle(expression(atop("Pearson Chi-Square p-values", paste("Tracts in SD; ", alpha, " = .05, Adjusted for Multiple Comparisons") )))+
##     labs(y = "log(p-value)", x = "Tract ID", col = "# Agents") + 
## scale_linetype_manual(name = "",values = 1,guide = "legend", lab = expression(log(alpha)))  + theme_light() +
##     theme(
##         axis.text.x = element_text(size = 10, family = "Palatino"),
##         axis.text.y= element_text(size = 10, family = "Palatino"),
##         axis.title.x= element_text(size = 16, family = "Palatino"),
##         axis.title.y= element_text(size = 16, family = "Palatino"),
##         plot.title = element_text(size = 20, family = "Palatino"),
##         legend.title = element_text(size = 16, family = "Palatino"),
##         legend.text = element_text(family = "Palatino")
##     )  
## #ggsave("~/Desktop/sd_pvals_region.pdf")
                         

## ## Think about binning variables...
## ## .bincode()


## ## ## Took 3 and a half minutes to run SD

## ## ## Cutting
## ## age <- PUMS_full$AGEP

## ## age_c <- cut(age, breaks=quantile(age, (1:10)/10, na.rm=TRUE) )




## ## How to include person/household weights
## ## TODO
## ## library("questionr")
## ## tab <- wtd.table(PUMS$RAC1P, weights = PUMS$PWGTP)
## ## tab2 <-table(PUMS$RAC1P)
## ## tab / sum(tab)
## ## tab2 / sum(tab2)
