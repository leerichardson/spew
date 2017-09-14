# SKG
# April 13, 2016
# Getting ACS data with the acs package
# Updated LFR, 8/17/2016

# Use spew and acs package to ACS summary tables  
library(acs)

#devtools::load_all()
# Replace this line with 
source("acs-tables.R")


# Loop through South Dakota counties downloading the 4 tables 
table_numbers <- c("B19001", "B11016", "B25007", "B25006")
table_ids <- c("HHInc", "HHSize", "HHHAge", "HHHRace")
counter <- 0

for (ii in 1:length(table_numbers)) {
  for (row in 1:nrow(fips.county)) {
    
    counter <- counter + 1
    if (counter %% 100 == 0) {
      print(counter)
      Sys.sleep(5)
    }

    st <- fips.county$State[row]
    co <- fips.county$County.ANSI[row]
    
    # Only download the South Dakota datasets 
    if (st != "SD") {
      next
    }
    
    geo_list <- list(state = st, county = co,  tract = "*")
    acs_fetch_list <- list(endyear = 2014, span = 5, table.number = table_numbers[ii])
    test <- fetchAndWriteACS(geo_list, acs_fetch_list, outPath = "data-raw/46/marginals", 
                             tabID = table_ids[ii])
  }
}
