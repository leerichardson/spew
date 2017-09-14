#' fetch and write ACS tables from acs package
#'
#' @param geo_list list with geographies for geo.make function
#' @param acs_fetch_list list with acs fetch data
#' @param outPath filepath
#' @param tabID way to ID table if NULL, use table.number in acs_fetch_list
#' @return df of acs and also a saved csv
fetchAndWriteACS <- function(geo_list, acs_fetch_list, outPath = ".", tabID = NULL){
  geo_obj <- geo.make(state = geo_list$state, county = geo_list$county,
                      tract = geo_list$tract)
  acs_obj <-  tryCatch({
    acs.fetch(endyear = acs_fetch_list$endyear,
              span = acs_fetch_list$span,
              geography = geo_obj,
              table.number = acs_fetch_list$table.number,
              col.names = "pretty")
  },
  error = function(e){print(paste("ERROR! State:", st, "County:", co))
    return(FALSE)}
  )
  if(class(acs_obj) == "logical" && !(acs_obj)) return(FALSE)
  col_nms <- acs_obj@acs.colnames
  acs_obj <- tryCatch({
    acs.fetch(endyear = acs_fetch_list$endyear,
              span = acs_fetch_list$span,
              geography = geo_obj,
              table.number = acs_fetch_list$table.number)
  },
  error = function(e){}
  )
  df <- makeACSdf(acs_obj)
  co <- formatID(geo = geo_list$county, type = "county")
  geoID <- paste(geo_list$state, co, sep = "-")
  acs_full <- list(df = df, col_nms = col_nms, geoID = geoID,
                   tableNum = acs_fetch_list$table.number, tabID = tabID)
  if (is.null(tabID)) tabID <- acs_fetch_list$table.number
  filename <- paste(geoID, tabID, sep = "_")
  filename <- paste0(filename, ".RDS")
  filepath <- file.path(outPath, filename)
  print(paste("Saving to", filepath))
  saveRDS(acs_full, filepath)
  return(acs_full)
}

#' make a more usable df from acs objects
#'
#' @param acs_obj class "acs" from acs package
#' @return a formatted df where the rows are the geographies and the columns are summary table values
#' @details to come
makeACSdf <- function(acs_obj){
  geoID <- makeGeoID(acs_obj@geography)
  est_df <- as.data.frame(acs_obj@estimate)
  est_df <- data.frame(geoID = geoID, est_df)
  rownames(est_df) <- NULL
  return(est_df)
}

#' Make a 11 digit tract ID
#'
#' @param acs_geo df from acs object's geography slot
#' @return 11 digit ID for each tract
#' @details The geographies should be US tracts
makeGeoID <- function(acs_geo){
  stopifnot(all(c("state", "county", "tract") %in% names(acs_geo)))
  acs_geo$state <- formatID(acs_geo$state, type = "state")
  acs_geo$county <- formatID(acs_geo$county, type = "county")
  acs_geo$tract <- formatID(acs_geo$tract, type = "tract")
  geoID <- paste0(acs_geo$state, acs_geo$county, acs_geo$tract)
  return(geoID)
}

#' Format the ids of a us state/county/tract
#'
#' @param geo IDs
#' @param type "state" "county" or "tract"
#' @return formatted state/county/tract ID
formatID <- function(geo, type){
  stopifnot(type %in% c("state", "county", "tract"))
  if (type == "state"){
    zeros <- 1 - floor(log10(as.numeric(geo)))
    new_geo <- sapply(1:length(geo), function(ind)
      paste0(rep("0", zeros[ind]), geo[ind]))
  } else if (type == "county"){
    zeros <- 2 - floor(log10(as.numeric(geo)))
    new_geo <- sapply(1:length(geo), function(ind)
      paste0(paste(rep("0", zeros[ind]), collapse = ""), geo[ind]))
  } else {
    tr <- gsub("[[:punct:]]", "", as.character(geo))
    new_geo <- sapply(tr, function(tr){
      if (nchar(tr) == 4){
        new_geo <- paste0(tr, "00")
      } else if (nchar(tr) == 5){
        new_geo <- paste0(tr, "0")
      } else {
        new_geo <- tr
      }
      return(new_geo)
    })
  }
  return(new_geo)   
}

#' Combine the household and people PUMS for IPF
#'
#' @param pums_p person pums from US ACS
#' @param pums_h household pums from US ACS (same year as person PUMS)
#' @param person_vars currently in c("AGEP", "RAC1P")
#' @param hh_vars currently in c("HINCP", "NP")
#' 
#' @return a shortened and combined set of PUMS
#' 
#' @references For details about the PUMS variables, 
#' see http://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2010-2014.pdf.  
combinePUMS <- function(pums_p, pums_h, person_vars = c("AGEP", "RAC1P"),
                        hh_vars = c("HINCP", "NP")) {
  p_sub <- pums_p[which(as.numeric(as.character(pums_p$RELP)) == 0), ] # get the head of household
  p_sub <- subset(p_sub, select = c("SERIALNO", "PUMA", "PWGTP", person_vars))
  hh_sub <- subset(pums_h, select = c("SERIALNO", "PUMA", hh_vars))
  pums_c <- plyr::join(p_sub, hh_sub)
  return(pums_c)
}

#' Format tables for use in IPF
#'
#' @param tab_obj from getting data from acs
#' @return the formatted table
formatTab_hhhAge <- function(tab_obj) {
  place_id <- as.character(tab_obj$df$geoID)
  removal_inds <- c(1, 2, 3, 13)
  new_tab <- tab_obj$df[, -removal_inds]
  col_nms <- c("HHH-15-24", "HHH-25-34", "HH-35-44", "HHH-45-54", 
               "HHH-55-59", "HHH-60-64", "HHH-65-74", "HHH-75-84", 
               "HHH-85-inf")
  new_mat <- new_tab[, 1:9] + new_tab[, 10:18]
  df <- data.frame(place_id, new_mat)
  colnames(df) <- c("place_id", col_nms)
  return(df)
}

#' Format tables for use in IPF
#'
#' @param tab_obj from getting data from acs
#' @return the formatted table
formatTab_hhhRace <- function(tab_obj){
  place_id <- as.character(tab_obj$df$geoID)
  new_tab <- tab_obj$df[, c(-1, -2)]
  race_names <- c("White", "Black", "Indian", "Asian", "Pacific", "Other", "Two+")
  two_plus <- rowSums(new_tab[, c(7:9)])
  new_mat <- cbind(new_tab[, 1:6], two_plus)
  df <- data.frame(place_id, new_mat)
  colnames(df) <- c("place_id", race_names)
  return(df)
}

#' Format tables for use in IPF
#'
#' @param tab_obj from getting data from acs
#' @return the formatted table
formatTab_hhInc <- function(tab_obj){
  place_id <- as.character(tab_obj$df$geoID)
  new_tab <- tab_obj$df[, c(-1, -2)]
  col_nms <- c("HH-0-10", "HH-10-15", "HH-15-25", "HH-25-35", "HH-35-50", "HH-50-100", "HH-100-200", "HH-200+")
  
  g1 <- new_tab[, 1]
  g2 <- new_tab[, 2]
  g3 <- rowSums(new_tab[, 3:4])
  g4 <- rowSums(new_tab[, 5:6])
  g5 <- rowSums(new_tab[, 7:9])
  g6 <- rowSums(new_tab[, 10:12])
  g7 <- rowSums(new_tab[, 13:15])
  g8 <- new_tab[, 16]
  
  new_mat <- cbind(g1, g2, g3, g4, g5, g6, g7, g8)
  df <- data.frame(place_id, new_mat)
  colnames(df) <- c("place_id", col_nms)
  
  return(df)
}

#' Format tables for use in IPF
#'
#' @param tab_obj from getting data from acs
#' @return the formatted table
formatTab_hhSize <- function(tab_obj){
  place_id <- as.character(tab_obj$df$geoID)
  removal_inds <- c(1, 2, 3, 10)
  new_tab <- tab_obj$df[, -removal_inds]
  
  col_nms <- c("HH-1", "HH-2", "HH-3", "HH-4", "HH-5", "HH-6", "HH-7+")
  g1 <- new_tab[, 7]
  g2 <- new_tab[, 1:6] + new_tab[, 8:13]
  new_mat <- cbind(g1, g2)
  df <- data.frame(place_id, new_mat)
  colnames(df) <- c("place_id", col_nms)
  return(df)
}

#' Sync the PUMS AGEP with the HHH Age var
#'
#' @param pums combined pums
#' @return pums with AGEP 
formatPUMS_hhhAge <- function(pums){
  age <- as.numeric(pums$AGEP)
  age_group <- sapply(age, function(age){
    if ( age >= 15 & age <= 24){
      return(1)
    } else if ( age >= 25 & age <= 34){
      return(2)
    } else if ( age >= 35 & age <= 44){
      return(3)
    } else if ( age >= 45 & age <= 54){
      return(4)
    } else if ( age >= 55 & age <= 59){
      return(5)
    }  else if ( age >= 60 & age <= 64){
      return(6)
    } else if ( age >= 65 & age <= 74){
      return(7)
    } else if ( age >= 75 & age <= 84){
      return(8)
    } else if ( age >= 85){
      return(9)
    }
    stop()
  })
  age_group <- factor(age_group, levels = 1:9,
                      labels = c("HHH-15-24", "HHH-25-34",
                                 "HHH-35-44", "HHH-45-54",
                                 "HHH-55-59", "HHH-60-64",
                                 "HHH-65-74","HHH-75-84",
                                 "HHH-85-inf")
  )
  
  pums$AGEP_ipf <- age_group
  return(pums)
}

#' Sync the PUMS AGEP with the HHH Race var
#'
#' @param pums combined pums
#' @return pums with RAC1P
formatPUMS_hhhRace <- function(pums){
  race <- as.numeric(pums$RAC1P)
  race_group <- sapply(race, function(race) {
    if (race == 1){
      return(1)
    } else if (race == 2) {
      return(2)
    } else if (race %in% c(3,4,5)){
      return(3)
    } else if (race == 6){
      return(4)
    } else if (race == 7){
      return(5)
    } else if (race == 8){
      return(6)
    } else if (race == 9){
      return(7)
    }
    stop()
  })
  race_group <- factor(race_group, levels = 1:7,
                       labels = c("White", "Black",
                                  "Native", "Asian",
                                  "Pacific", "Other",
                                  "Two+")
  )
  pums$RAC1P_ipf <- race_group
  return(pums)
}

#' Sync the PUMS HINCP with the HHH Race var
#'
#' @param pums combined pums
#' @return pums with HINCP
formatPUMS_hhInc <- function(pums){
  inc <- floor(as.numeric(pums$HINCP)/ 1000)
  inc_group <- sapply(inc, function(inc){
    if (is.na(inc)){
      return(1)
    } else if (inc < 10){
      return(1)
    } else if (inc >= 10 & inc < 15){
      return(2)
    } else if (inc >= 15 & inc < 25){
      return(3)
    } else if (inc >= 25 & inc < 35){
      return(4)
    } else if (inc >= 35 & inc < 50){
      return(5)
    } else if (inc >= 50 & inc < 100){
      return(6)
    } else if (inc >= 100){
      return(7)
    }
    stop()
  })
  inc_group <- factor(inc_group, levels = 1:7,
                      labels = c("HH-0-10", "HH-10-15",
                                 "HH-15-25", "HH-25-35",
                                 "HH-35-50", "HH-50-100",
                                 "HH-100-inf")
  )
  
  
  pums$HINCP_ipf <- inc_group
  return(pums)
}

#' Sync the PUMS NP with the HH Size var
#'
#' @param pums combined pums
#' @return pums with NP
formatPUMS_hhSize <- function(pums){
  size<- as.numeric(pums$NP)
  size_group <- sapply(size, function(size){
    if (size == 1){
      return(1)
    } else if  (size  == 2){
      return(2)
    } else if  (size  == 3){
      return(3)
    } else if  (size  == 4){
      return(4)
    } else if  (size  == 5){
      return(5)
    } else if  (size  == 6){
      return(6)
    } else if  (size  >= 7){
      return(7)
    }
    print(size)
    stop()
  })
  size_group <- factor(size_group, levels = 1:7,
                       labels = c("HH-1", "HH-2",
                                  "HH-3", "HH-4",
                                  "HH-5", "HH-6",
                                  "HH-7+")
  )
  pums$NP_ipf <- size_group
  return(pums)
}
