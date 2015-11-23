#  Sam Ventura, Lee Richardson, Shannon Gallagher

#  Pseudocode ------------------------

#  Input: List of all data inputs 

#  Assert that to make sure we have the correct inputs. (at least the first three)

#  1. Pull out a vector of the poptable IDs and Shapefile IDs
#  2. Join to make sure they match (Sam will do this one)
#  3. Make sure the PUMA's is included in the pop_table (using the lookup table), 
#  contain this WITHIN THE DATA GROUP. Puerto Rico idea 
#  4. Output a table with the specified dimensions 

# Output: Pop Table with tract ID, county ID, PUMA ID, # households in tract.
# Table (PUMS sampling ID, Place ID, Number of households to sample)

#  Output is a table with:
##  Rows:  All individual tracts in the region to be generated
##  Columns:  PlaceID (tract ID), pumaID (puma that contains the tract), # of households

#  Make sure that the tract IDs match with the shapefile IDs 
##  For data_group = "US", this doesn't really matter
##  For data_group = "ipums", this is important
##  Need to record-link the Place names across shapefiles and other sources

format_data <- function(data_list, data_group) {
    
    if (data_group == "US") {
      #  Do this now
    } else if (data_group == "ipums") {
      #  Do this later
    }
}
