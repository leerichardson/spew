#  Sam Ventura, Lee Richardson, Shannon Gallagher

#  Pseudocode ------------------------

#  Input:  List of all data inputs 

#  Assert that we have the correct inputs. (at least the first three, four for US)

#  1. Pull out a vector of the poptable IDs and Shapefile IDs
#  2. Join to make sure they match (Sam will do this one)
#  3. Make sure the PUMA's is included in the pop_table (using the lookup table), 
#  contain this WITHIN THE DATA GROUP. Puerto Rico idea 
#  4. Output a table with the specified dimensions 

#  Output: Pop Table with tract ID, county ID, PUMA ID, # households in tract.
#  Table (PUMS sampling ID, Place ID, Number of households to sample)

#  Output is a table with:
##  Rows:  All individual places (tracts) in the region to be generated
##  Columns:  PlaceID (tract ID), pumaID (puma that contains the tract), # of households

#  Make sure that the tract IDs match with the shapefile IDs 
##  For data_group = "US", this doesn't really matter
##  For data_group = "ipums", this is important
##  Need to record-link the Place names across shapefiles and other sources
format_data <- function(data_list, data_group) {
    
  # Assert that we have all the REQUIRED (shapefile, pums, counts) elements 
  # for generating the synthetic population....
  
  if (data_group == "US") {
            
      # Pull out a vector of the poptable IDs and Shapefile IDs
      link_pop_table_to_lookup <- match(data_list$pop_table$place_id, data_list$lookup$place_id)
      data_list$pop_table$puma_id <- data_list$lookup$puma_id[link_pop_table_to_lookup]
      
  } else if (data_group == "ipums") {

  }
  
  return(data_list)
}
