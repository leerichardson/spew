# Pseudocode ------------------------

# Input: List of all data inputs 

# Assert that to make sure we have the correct inputs. (at least the first three)

# 1. Pull out a vector of the poptable IDs and Shapefile IDs
# 2. Join to make sure they match (Sam will do this one)
# 3. Make sure the PUMA's is included in the pop_table (using the lookup table), 
# contain this WITHIN THE DATA GROUP. Puerto Rico idea 
# 4. Output a table with the specified dimensions 

# Output: Pop Table with tract ID, county ID, PUMA ID, # households in tract.
# Table (PUMS sampling ID, Place ID, Number of households to sample)

# Make sure that the tract ID's match with the shapefile ID's 

# "Households.csv Specifies the number of households in each tract"
format <- function(data_list, data_group) {
    
    if (data_group == "US") {
      
    } else if (data_group == "ipums") {
      
    }
}

