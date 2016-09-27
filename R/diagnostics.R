
#' Run basic 'sanity check' diagnostics on a folder of SPEW generated populations.  There should only be one household and one people file in each folder.  If not, look into append_pops().
#' 
#' @param input_dir path to the directory (bottom level) of populations to summarize
#' @param output_dir path to output directory
#' @param save_plots boolean indicating whether the plots should be saved
#' @param pretty if set to TRUE, should generate a summary pdf including plots (currently non-functional)
#' @return  a print out of the files generated

run_diags <- function(input_dir="./", output_dir=input_dir, save_plots=TRUE, pretty=FALSE){
    #read in filenames
    filenames <- list.files(input_dir)
    filename_hh <- filenames[ grep("household", filenames) ]
    filename_p <- filenames[ grep("people", filenames) ]
    stopifnot(length(filename_hh) * length(filename_p) == 1)
    hh_df <- read.csv(paste0(input_dir, filename_hh))
    # write log file
    
    puma_id <- unique(hh_df$puma_id)
    log_filename <- paste0("log_", puma_id, ".txt")
    if(!prettY){
      sink(paste0(output_dir, log_filename))
    }
    print(paste("PUMA ID:", puma_id))
    print("\n")
    print("HOUSEHOLDS")
    print("\n")
    print(paste("Number of households:", nrow(hh_df)))
    print("\n")
    print("Household column names:")
    print(colnames(hh_df))
    print("\n")
    print(paste("Average Longitude:", round(mean(hh_df$long),3)))
    print(paste("Average Latitude:", round(mean(hh_df$lati),3)))
    print("\n")
    print(paste("Number of unique households sampled:", length(unique(hh_df$SERIALNO))))
    print("\n")
    if(!pretty){
      sink()
    }
    
    rm(hh_df)

    #the people information
    p_df <- read.csv(paste0(input_dir, filename_p))
  
    if(!pretty){
      sink(paste0(output_dir, log_filename), append=TRUE)
    }
    print("PEOPLE")
    print("\n")
    print(paste("Number of individuals:", nrow(p_df)))
    print("\n")
    print("Column names:")
    print(colnames(p_df))
    mfr<-sum(p_df$SEX == 1)/sum(p_df$SEX == 2)
    print(paste("Male to Female Ratio:", round(mfr, 2)))
    if(!pretty){
    sink()
    }
    return(row)

}


#' Sumarize the files within the output dir
#'
#' @param output_dir path to output of SPEW region name
#' @param doPrint logical
#' @param type either "ipums" or "us"
#' @return list
summarizeFileStructure <- function(output_dir, doPrint = FALSE, type = "ipums"){
    stopifnot(type %in% c("ipums", "us"))
    # Region Name
    base_region <- gsub("output_", "", basename(output_dir))
    pretty_print(doPrint, paste("The region is", toupper(base_region)))
    paths <- list.files(output_dir, recursive = T)
    output_paths <- paths[grepl("output", paths)]
    if (length(output_paths) < 1){
           hh_paths <- paths[grepl("household", paths)]
    } else {
        hh_paths <- output_paths[grepl("household", output_paths)]
    }
    paths_df <- pathsToDF(hh_paths)
    # Number of levels in file hierarchy
    nLevels <- getLevels(paths_df)
    pretty_print(doPrint, paste("There are", nLevels,
                                "level(s) of nested ecosystems in this region."))
    nRegions <- nrow(paths_df)
    pretty_print(doPrint, paste("There are", nRegions, "lowest level subregions in", toupper(base_region)))
    return(list(base_region = base_region, paths_df = paths_df, nLevels = nLevels))
}

#' Convert paths to data frame based on folders
#'
#' @param paths paths separated by a single /
#' @return df of paths
pathsToDF <- function(paths){
    ll <- strsplit(paths, "/")
    nLevels <- max(sapply(ll, length))
    inds <- sapply(ll, function(val) length(val) == nLevels)
    mat <- do.call(rbind, ll[inds])
    df <- as.data.frame(mat)
    colnames(df) <- paste0("folder", 1:ncol(df))
    return(df)
}




#' Print text if doPrint
#'
#' @param doPrint logical
#' @param text string
#' @return print statement
pretty_print <- function(doPrint = TRUE, text){
    if (doPrint){
        print(text)
    }
}


#' Get the number of levels in the structure
#'
#' @param paths_df paths df
#' @return number of levels in file path
getLevels <- function(paths_df){
    if (ncol(paths_df) < 3){
        return(1)
    }else if (ncol(paths_df) == 3){
        un1 <- length(unique(paths_df[, 1]))
        un2 <- length(unique(paths_df[, 3]))
        if (un1 == un2){
            return(1)
        } else {
            return(2)
        }
    } else {
        return( floor(ncol(paths_df) / 2) + 1)
    }
}

#' Extract the StCoTr number and put it in a df
#'
#' @param paths_df data frame last column has stco tr number in it
#' @return data frame
extractStCoTr <- function(paths_df){
    stcotr_col <- paths_df[, ncol(paths_df)]
    stcotr <- gsub("[^0-9]", "", stcotr_col)
    stopifnot(all(nchar(stcotr) == 11))
    st <- substr(stcotr, 1, 2)
    co <- substr(stcotr, 3, 5)
    tr <- substr(stcotr, 6, 11)
    df <- data.frame(state = st, county = co, tract = tr, stringsAsFactors = FALSE)
    return(df)
}


#' Summarize us populations
#'
#' @param output_dir path
#' @param us_fs output from summarizeFileStructure
# #' @param shapefile_path default is NA, uses output_dir
#' @param varsToSummarize list with first entry as vector of household ipums vars and the second as person ipums vars.  The default value is 'base' which includes total number of records.  For households 'base' includes longitude and latitude and the names.  For people, this includes gender.  Otherwise we summarize the base variables and the the variable names which we should summarize.  For the summary output to be something other than a factor variable then see var_list.
#' @param doPrint logical
#' @param sampSize number of people to retain (default is 10000) per region for plotting
#' @param sum_level 1 - state, 2- country or 3- tract - the subregions which we summarize and eventually plot
#' @return list
summarize_us <-  function(output_dir, us_fs,
                             varsToSummarize = list(
                                 vars_hh = "base",
                                 vars_p = "base"
                             ),
                          doPrint = FALSE,
                          sampSize = 10^4,
                          sum_level = 2
                          ){
    # Get the data frame for use
    paths_df <- us_fs$paths_df
    stcotr <- extractStCoTr(paths_df)
    
    # TODO switch this to us
    vars_hh <- getVars_ipums(varsToSummarize$vars_hh, type = "hh")
    vars_p <- getVars_ipums(varsToSummarize$vars_p, type = "p")
    hh_sum_list <- vector(mode = "list", length = nrow(paths_df))

     #sum level is 1 (st) 2 (co) 3 (tr)
    sum_col <- unique(stcotr[, 1:sum_level] )
    # Aggregate!!
    if (is.null(dim(sum_col))){
        nSubReg <- 1
        sum_col <- as.data.frame(sum_col, stringsAsFactors = FALSE)
    } else {
        nSubReg <- nrow(sum_col)
    }
    
    # Loop through the AGGREGATED regions to summarize the households
    pretty_print(doPrint, "Summarizing households!")
    hh_sum_list <- vector(mode = "list", length = nSubReg)
    header <- NULL  ## HOUSEHOLDS!!
    for (lev in 1:nSubReg){
        # Lowest level regions in subregion
        reg <- paste(sum_col[lev, ], collapse= "")
        if (nSubReg > 1){
            all_reg <- apply(stcotr[, 1:sum_level], 1,
                         paste, collapse = "")
        } else {
            all_reg <- stcotr[, sum_level]
        }
        reg_inds <- which(all_reg == reg)

        ## Get the Full file path(s)

        paths_df <- data.frame(lapply(paths_df, as.character), stringsAsFactors=FALSE)
        fp <- sapply(reg_inds, function(ind) paste(paths_df[ind, ], collapse = "/"))
        
        # Read in the lowest level csvs
        tab <- do.call('rbind', lapply(file.path(output_dir, fp), function(fp){
            tab <- read.csv(fp)[, c(vars_hh$cont, vars_hh$cat, "longitude", "latitude")]
            return(tab)
            }))

        # Summarize the features, first categorical then cont.
        sum_features_cat <- sapply(vars_hh$cat, summarizeFeatures,
                                   tab, type = "cat")
        sum_features_cont <- sapply(vars_hh$cont, summarizeFeatures, tab, type = "cont")
        sum_features <- list(cat = sum_features_cat,
                             cont = sum_features_cont)
        # Extract the header
        header_hh<- colnames(tab)
        # Sample a portion of them to plot
       # sampSize <- ifelse(sampSize > nrow(tab),
           #                nrow(tab), sampSize)
        sub_inds <- sample(1:nrow(tab), sampSize, replace = T)
        sub_df <- subset(tab[sub_inds,], select = c("longitude", "latitude"))
        # Extract the useful details
        region_id<- reg # TODO get the pretty county name
        region_no <- reg #
        pretty_print(doPrint, region_id)
        if ("longitude" %in% colnames(tab)){
            coords <- colMeans(tab[, c("longitude", "latitude")])        
        }
        # Put region specific summaries in a df
        region_sum <- data.frame(region_id = region_id,
                                 region_no = region_no,
                                 nRecords = nrow(tab),
                                 avg_lon = coords[1],
                                 avg_lat = coords[2])
        rownames(region_sum) <- NULL
        place_id <- region_id
        # Return a list of the details
        hh_sum_list[[lev]] <- list(region_sum = region_sum,
                                   sum_features = sum_features,
                                   sub_df = sub_df,
                                   header = header_hh,
                                   place_id = place_id)

    }
    p_sum_list <- vector(mode = "list", length = nSubReg)
    ### PEOPLE!!
    pretty_print(doPrint, "Summarizing people!")
    for (lev in 1:nSubReg){
        # Lowest level regions in subregion
        reg <- paste(sum_col[lev, ], collapse= "")
        if (nSubReg > 1){
            all_reg <- apply(stcotr[, 1:sum_level], 1,
                         paste, collapse = "")
        } else {
            all_reg <- stcotr[, sum_level]
        }
        reg_inds <- which(all_reg == reg)
        ##                      # Change the paths from household to people
        paths_df_p <- paths_df
        paths_df_p[, ncol(paths_df)] <- gsub("household", "people", paths_df[, ncol(paths_df)])
##        paths_df <- data.frame(lapply(paths_df, as.character), stringsAsFactors=FALSE)
        # Get the Full file path(s)
        fp <- sapply(reg_inds, function(ind) paste(paths_df_p[ind, ], collapse = "/"))
        tab <- do.call('rbind', lapply(file.path(output_dir, fp),  function(fp){
            tab <- read.csv(fp)
            tab <- tab[, c(vars_p$cont, vars_p$cat, "puma_id", "place_id")]
            return(tab)
            }
            ))
        header_p<- colnames(tab)
        # Summarize the features
        sum_features_cat <- sapply(vars_p$cat, summarizeFeatures, tab, type = "cat")
        sum_features_cont <- sapply(vars_p$cont, summarizeFeatures, tab, type = "cont")
        sum_features <- list(cat = sum_features_cat,
                             cont = sum_features_cont)
      #  sampSize <- ifelse(sampSize > nrow(tab), nrow(tab), sampSize)
        sub_inds <- sample(1:nrow(tab), sampSize, replace = T)
#        regionID <- gsub("people_", "", basename(fp))
        region_id<- reg
        region_no <- reg
        pretty_print(doPrint, region_id)
        if ("longitude" %in% colnames(tab)){
            coords <- colMeans(tab[, c("longitude", "latitude")])        
        }
        place_id <- region_id
        region_sum <- data.frame(region_id = region_id,
                                 region_no = region_no,
                                 nRecords = nrow(tab))
        rownames(region_sum) <- NULL
        p_sum_list[[lev]] <- list(region_sum = region_sum,
                                   sum_features = sum_features,
                                  place_id = place_id)

    }
    
    return(list(hh_sum_list = hh_sum_list,
                header_hh = header_hh, p_sum_list = p_sum_list,
                header_p = header_p))
}




#' Summarize ipums populations
#'
#' @param output_dir path
#' @param ipums_fs output from summarizeFileStructure
# #' @param shapefile_path default is NA, uses output_dir
#' @param varsToSummarize list with first entry as vector of household ipums vars and the second as person ipums vars.  The default value is 'base' which includes total number of records.  For households 'base' includes longitude and latitude and the names.  For people, this includes gender.  Otherwise we summarize the base variables and the the variable names which we should summarize.  For the summary output to be something other than a factor variable then see var_list.
#' @param doPrint logical
#' @param sampSize number of people to retain (default is 10000) per region for plotting
summarize_ipums <-  function(output_dir, ipums_fs,
                             varsToSummarize = list(vars_hh = "base", vars_p = "base"),
                             doPrint = FALSE, sampSize = 10^3){
    stopifnot(ncol(ipums_fs$paths_df) == 3)
    paths_df <- ipums_fs$paths_df
    paths_df <- data.frame(lapply(paths_df, as.character), stringsAsFactors = FALSE)
    vars_hh <- getVars_ipums(varsToSummarize$vars_hh, type = "hh")
    vars_p <- getVars_ipums(varsToSummarize$vars_p, type = "p")
    hh_sum_list <- vector(mode = "list", length = nrow(paths_df))
    pretty_print(doPrint, "Summarizing households!")
    header <- NULL  ## HOUSEHOLDS!!
    for (ind in 1:nrow(paths_df)){
        fp <- paste(paths_df[ind, ], collapse = "/")
        tab <- read.csv(file.path(output_dir, fp))
        sum_features_cat <- sapply(vars_hh$cat, summarizeFeatures, tab, type = "cat")
        sum_features_cont <- sapply(vars_hh$cont, summarizeFeatures, tab, type = "cont")
        sum_features <- list(cat = sum_features_cat,
                             cont = sum_features_cont)
        header_hh<- colnames(tab)
       # sampSize <- ifelse(sampSize > nrow(tab), nrow(tab), sampSize)
        sub_inds <- sample(1:nrow(tab), sampSize, replace = TRUE)
        sub_df <- subset(tab[sub_inds,], select = c("longitude", "latitude"))
       # print(dim(sub_df))
        region_id <- gsub("household_", "",  paths_df[ind, 3])
        region_id <- gsub(".csv", "", region_id)
        region_no <- gsub("output_", "", paths_df[ind, 1])
        pretty_print(doPrint, region_id)
        if ("longitude" %in% colnames(tab)){
            coords <- colMeans(tab[, c("longitude",
                                       "latitude")])        
        }
        region_sum <- data.frame(region_id = region_id,
                                 region_no = region_no,
                                 nRecords = nrow(tab),
                                 avg_lon = coords[1],
                                 avg_lat = coords[2])
        rownames(region_sum) <- NULL
        hh_sum_list[[ind]] <- list(region_sum = region_sum,
                                   sum_features = sum_features,
                                   sub_df = sub_df,
                                   header = header_hh)        
    }
    p_sum_list <- vector(mode = "list", length = nrow(paths_df))
    ### PEOPLE!!
    pretty_print(doPrint, "Summarizing people!")
    for (ind in 1:nrow(paths_df)){
        paths_df_p <- paths_df
        paths_df_p[, ncol(paths_df)] <- gsub("household", "people", paths_df[, ncol(paths_df)])
        fp <- paste(paths_df_p[ind, ], collapse = "/")
        tab <- read.csv(file.path(output_dir, fp))
        sum_features_cat <- sapply(vars_p$cat, summarizeFeatures, tab, type = "cat")
        sum_features_cont <- sapply(vars_p$cont, summarizeFeatures, tab, type = "cont")
        sum_features <- list(cat = sum_features_cat,
                             cont = sum_features_cont)
        header_p<- colnames(tab)
        sub_inds <- sample(1:nrow(tab), sampSize, replace = T)
        region_id <- gsub("household_", "",  paths_df[ind, 3])
        region_id <- gsub(".csv", "", region_id)
        region_no <- gsub("output_", "", paths_df[ind, 1])
        place_id <- region_id
        pretty_print(doPrint, region_id)
        if ("longitude" %in% colnames(tab)){
            coords <- colMeans(tab[, c("longitude", "latitude")])        
        }
        region_sum <- data.frame(region_id = region_id,
                                 region_no = region_no,
                                 nRecords = nrow(tab))
        rownames(region_sum) <- NULL
        p_sum_list[[ind]] <- list(region_sum = region_sum,
                                   sum_features = sum_features)

    }
    return(list(hh_sum_list = hh_sum_list,
                header_hh = header_hh, p_sum_list = p_sum_list,
                place_id = place_id, header_p = header_p))
}


#' Add the base variables to the vars to summarize
#'
#' @param summary_vars character vector with variables matching the microdata
#' @param type "hh" for household or "p" for people
#' @return list of the categorical and continuous variables na mes
getVars_ipums <- function(summary_vars, type){
     if ( type == "hh"){
        if( summary_vars == "base"){
            var_names <- NULL
        } else{
            var_names <- summary_vars
        }
    } else {
        if (summary_vars == "base"){
            var_names <- c("SEX")
        } else {
            var_names <- c("SEX", summary_vars)
        }
    }
    return(list(cat = var_names, cont = NULL))
}

#' Summarizing a synthetic ecosystem
#'
#' @param var variable name
#' @param tab dataframe of the syneco
#' @param type "cat" or "cont" for categorical or continuous data.  right now continuous data is not summarized
#' @return summary info
summarizeFeatures <- function(var, tab, type = "cat"){
    if (is.null(var)){
        return(0)
    } else if ( type == "cat"){
        sum_tab <- table(tab[, var])
        names(sum_tab) <- paste0(var, "-", names(sum_tab))
        return(sum_tab)                                 
    } else {
        return(0)
    }
}

#' Plot the region diagnostics
#'
#' @param ipums_sum_list output from summarize_ipums()
#' @param ipums_fs output from summarizeFileStructure()
#' @param pretty ggplot or base R
#' @param borders logical inclusion of shapefile boundaries
#' @param data_path input directory to plot the borders
#' @param savePlot logical
#' @param plot_name what to save the plot as
#' @param map_type for ggplot use, default is toner-lite
#' @param diags_path path to diags folder
#' @param type either "ipums" or "us"
#' @param ... plotting params
#' @return logical or ggplot object
plot_region_diags<- function(ipums_sum_list, ipums_fs, pretty = TRUE, borders = FALSE,
                        data_path = input_dir, savePlot = FALSE, plot_name,
                        map_type = "toner-lite", diags_path = ".",
                        type = "ipums", ...){
    region <- toupper(ipums_fs$base_region)
    plot_df <- makePlotDF(ipums_sum_list)
    centers_df <- getCentersDiags(ipums_sum_list)
    if (type == "us"){
        centers_df$reg <- sapply(centers_df$reg,
                                 fipsToPlaceName, level = "county", df = us)
    }
    nRegions <- length(unique(plot_df$reg))
    if (borders){
        bds <- getBorders(data_path, ipums_sum_list, ipums_fs)
    }
    if (pretty){
        bbox <- make_bbox(plot_df$lon, plot_df$lat, f =.25)
        map <- get_map(bbox, maptype = map_type)
        #cols <- brewer.pal(nRegions, "Spectral")
        #names(cols) <- levels(plot_df$reg)
       # The palette with grey:
        cbbPalette <- c("#999999", "#E69F00", "#56B4E9",
                       "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        cols <- rep(cbbPalette, length.out = nRegions)
        #cols <- cbbPalette[1:nRegions]
        colScale <- scale_colour_manual(name = "reg", values = cols)
        g <- ggmap(map) + geom_point(data = plot_df,
                                aes(x = longitude, y = latitude, colour = factor(reg)),
                                cex = .4) +
            geom_text(data = centers_df, aes(x = avg_lon, y = avg_lat, label = reg), size = 3) + #ggtitle(region) + 
              colScale +
#            guides(colour = guide_legend(title = "Region", override.aes = list(size = 10))) +
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  legend.position = "none",
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank())
        print(g)
        if(savePlot){
            ggsave(file.path(data_path, plot_name))
        }
        return(g)
    } else {
        # TODO
    }
    

}

#' Convert ipums_sum_list into a data frame for plotting
#'
#' @param ipums_sum_list  output from summarize_ipums()
#' @return data frame for plotting
makePlotDF <- function(ipums_sum_list){
    hh_sum_list <- ipums_sum_list$hh_sum_list
    nRecords <- do.call('rbind', lapply(hh_sum_list, "[[", 1))$nRecords
    nRecsP <- nRecords / sum(nRecords)
   # print(nRecsP)
    nRegions <- length(hh_sum_list)
   # nRecsMax <- max(lapply(lapply(hh_sum_list, "[[", 3), nrow))
    plot_df <- NULL
    for ( reg in 1:nRegions){
        ll <- hh_sum_list[[reg]]
        reg_name <- ll$region_sum$region_id
        df <- ll$sub_df
        nInds <- floor(nRecsP[reg] * nrow(df))
                                        # print(nInds)
        if(nInds > 0){
            inds <- sample(1:nrow(df), nInds)
            sub_df <- df[inds, ]
            sub_df$reg <- toupper(reg_name)
        }
        plot_df <- rbind(plot_df, sub_df)
    }
    return(plot_df)
}


#' Get the centers df for each region
#'
#'  @param ipums_sum_list  output from summarize_ipums()
#' @return data frame for plotting
getCentersDiags <- function(ipums_sum_list){
    hh_sum_list <- ipums_sum_list$hh_sum_list
    nRegions <- length(hh_sum_list)
    plot_df <- NULL
    for ( reg in 1:nRegions){
        ll <- hh_sum_list[[reg]]
        df <- ll$region_sum
        df$reg <- toupper(df$region_id)
        plot_df <- rbind(plot_df, df)
    }
    return(plot_df)

}

#' Aggregate us tracts

aggregate_us <- function(us_list, sum_level){
    nRegions <- length(us_list)

}

#' Translate FIPS number to place name
#'
#' @param fips string id
#' length 2 for state
#' length 5 for county
#' @param level ("state", "county")

fipsToPlaceName <- function(fips, level, df = us){
 #   print("FIPS code is")
 #   print(fips)
    stopifnot(level %in% c("state", "county"))
    stopifnot(!is.null(df))
    if (level == "state"){
        ind <- which(as.character(df$STATEFP) == fips)
        state <- df$STATE_NAME[ind][1]
        return(state)
    } else {
        stopifnot(nchar(fips) == 5)
        ind <- which(as.character(df$COUNTYFP) == fips)
        stopifnot(length(ind) > 0)
        pn <- df$County[ind][1]
        pn <- gsub(" County", "", pn)
        return(pn)
    }
}
