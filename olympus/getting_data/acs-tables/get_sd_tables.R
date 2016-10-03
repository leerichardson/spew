# SKG
# April 13, 2016
# Getting ACS data with the acs package
# Updated LFR, 8/17/2016

# Use spew and acs package to ACS summary tables  
library(acs)
library(spew)

# Loop through South Dakota counties downloading the 4 tables 
table_numbers <- c("B19001", "B11016", "B25007", "B25006")
table_ids <- c("HHInc", "HHSize", "HHHAge", "HHHRace")

# Get the state lookup table and set the us directory
state_lookup <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/input/west/north_america/united_states/states_lookup.csv")
us_dir <- "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa/"
state_ids <- state_lookup[, "geoid"]

state_dirs <- list.dirs(us_dir, recursive = FALSE, full.names = FALSE)

for (row in 1:nrow(fips.county)) {
    for (ii in 1:length(table_numbers)) {


    st <- fips.county$State[row]
    co <- fips.county$County.ANSI[row]
    print(paste0("State: ", st, " County: ", co, " Var: ", table_ids[ii]))
    
    state_ind <- which(state_lookup$state_abbrev == tolower(st))
    state_id <- as.character(state_ids[state_ind])
    
    # Add an extra 0 to the state ID if it is less than 10 
    if (nchar(state_id) == 1) {
      state_id <- paste0("0", state_id)
    }
    
    # Skip if it's not a State or Puerto Rico!    
    avail_dir <- state_id %in% state_dirs
    if (!avail_dir) {
        next
    }

    if (as.numeric(state_id) < 36) {
        next
    }

    marginal_loc <- paste0(us_dir, state_id, "/input/marginals/natstat/2014/tract")
    dir.create(marginal_loc, recursive = TRUE)
    
    # Create marginals directory to store the marginals
    geo_list <- list(state = st, county = co,  tract = "*")
    acs_fetch_list <- list(endyear = 2014, span = 5, table.number = table_numbers[ii])
    test <- spew:::fetchAndWriteACS(geo_list, acs_fetch_list, outPath = marginal_loc, tabID = table_ids[ii])
  }
}
