# SKG
# April 14, 2016
# Format PUMS to make a df that matches the summary tables
# Summary Tables:
# http://www.epimodels.org/10_Midas_Docs/SynthPop/2010_synth_pop_ver1_quickstart.pdf
# PUMS:
# http://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2010-2014.pdf
# Updated LFR, 8/17/2016
library(spew)

# Get a list of the files, counties, and variables
format_marginals <- function(base_path) {

  # Clean the old marginals file 
  # file.remove(paste0(base_path, "/marginals.RDS"))

  # Obtain a list of all the marginal files in this directory 
  summary_files <- list.files(base_path)
  n <- length(summary_files)
  counties <- substr(summary_files, 4, 6)
  vars <- substr(summary_files, 8, 13)

  # Set up the ideal morgnial list 
  marginals <- list(AGEP = list(df = NULL, type = NULL, lookup = NULL), 
                    RAC1P = list(df = NULL, type = NULL, lookup = NULL), 
                    HINCP = list(df = NULL, type = NULL, lookup = NULL), 
                    NP = list(df = NULL, type = NULL, lookup = NULL))

  # Loop through each var-county, load and format 
  # the table, and append to the marginal list ---
  uniq_vars <- unique(vars)
  uniq_vars <- uniq_vars[grep("HH", uniq_vars)]

  uniq_counties <- unique(counties)
  uniq_counties <- uniq_counties[-which(is.na(as.numeric(uniq_counties)))]


  # Correct if we are fixing a previous saved version
  uniq_vars <- uniq_vars[grep("HH", uniq_vars)]

  for (var in uniq_vars) {
    for (county in uniq_counties) {
      # Extract county-variable specific filename 
      var_inds <- which(var == vars)
      county_inds <- which(county == counties)
      var_count_ind <- intersect(var_inds, county_inds)
      filepath <- summary_files[var_count_ind]
      print(filepath)
      
      # Load and format summary tables -----
      tab <- readRDS(paste0(base_path, "/", filepath))
      if (var == "HHHAge") {
        format_tab <- spew:::formatTab_hhhAge(tab)
        
        # If first table, start df. If not, append
        if (is.null(marginals$AGEP$df)) {
          marginals$AGEP$df <- format_tab
        } else {
          marginals$AGEP$df <- rbind(marginals$AGEP$df, format_tab)
        }
        
        # Set the type      
        marginals$AGEP$type <- "ord"
        
        # Create the Lookup Table 
        marg_names <- names(marginals$AGEP$df)[-1]
        lower <- c(15, 25, 35, 45, 55, 60, 65, 75, 85)
        upper <- c(24, 34, 44, 54, 59, 64, 74, 84, 10000)
        lookup <- data.frame(marg_names, lower, upper, stringsAsFactors = FALSE)
        marginals$AGEP$lookup <- lookup   
        
      } else if (var == "HHHRac") {
        format_tab <- spew:::formatTab_hhhRace(tab)
        
        # If first table, start df. If not, append
        if (is.null(marginals$RAC1P$df)) {
          marginals$RAC1P$df <- format_tab
        } else {
          marginals$RAC1P$df <- rbind(marginals$RAC1P$df, format_tab)
        }
        
        # Set the type      
        marginals$RAC1P$type <- "cat"
        
        # Create the Lookup Table 
        marg_names <- names(marginals$RAC1P$df)[-1]
        lower <- c(1, 2, 3, 6, 7, 8, 9)
        upper <- c(1, 2, 5, 6, 7, 8, 9)
        lookup <- data.frame(marg_names, lower, upper, stringsAsFactors = FALSE)
        marginals$RAC1P$lookup <- lookup
        
        
      } else if (var == "HHInc.") {
        format_tab <- spew:::formatTab_hhInc(tab)
        
        # If first table, start df. If not, append
        if (is.null(marginals$HINCP$df)) {
          marginals$HINCP$df <- format_tab
        } else {
          marginals$HINCP$df <- rbind(marginals$HINCP$df, format_tab)
        }
        
        # Set the type      
        marginals$HINCP$type <- "ord"
        
        # Create the Lookup Table 
        marg_names <- names(marginals$HINCP$df)[-1]
        lower <- c(-60000, 10000, 15000, 25000, 35000, 50000, 100000, 200000)
        upper <- c(9999, 14999, 24999, 34999, 49999, 99999, 199999, 100000000)
        lookup <- data.frame(marg_names, lower, upper, stringsAsFactors = FALSE)
        marginals$HINCP$lookup <- lookup
        
      } else if (var == "HHSize") {
        format_tab <- spew:::formatTab_hhSize(tab)
      
        # If first table, start df. If not, append
        if (is.null(marginals$NP$df)) {
          marginals$NP$df <- format_tab
        } else {
          marginals$NP$df <- rbind(marginals$NP$df, format_tab)
        }
        
        # Set the type      
        marginals$NP$type <- "ord"
        
        # Create the Lookup Table 
        marg_names <- names(marginals$NP$df)[-1]
        lower <- c(1, 2, 3, 4, 5, 6, 7)
        upper <- c(1, 2, 3, 4, 5, 6, 10000)
        lookup <- data.frame(marg_names, lower, upper, stringsAsFactors = FALSE)
        marginals$NP$lookup <- lookup
        
      } else {
        stop("Not a correct variable name")
      }

    }
  }

  # Save the marginal list object -----
  saveRDS(object = marginals, file = paste0(base_path, "/marginals.RDS"))
}

# Call the format_marginals function on ALL States ---------------------

# Get a list of state IDs
states <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/spew_hierarchy/country_hierarchies/usa_lookup.csv")
ids <- as.character(states[, 1])

# Set the path in the new directory and to the marginal directory 
us_dir <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa/"
marg_path <- "/input/marginals/natstat/2014/tract"

# For each state, call the "format marginals" function 
for (state in ids) { 
    # Add an extra 0 to the state ID if it is less than 10 
    if (nchar(state) == 1) {
      state <- paste0("0", state)
    }

    if (state > 19) {
      next
    }

    # Contruct the path and call the function 
    state_path <- paste0(us_dir, state, marg_path)
    print(state_path)
    format_marginals(state_path)
}

