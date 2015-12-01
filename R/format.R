#  Sam Ventura, Lee Richardson, Shannon Gallagher

#  Pseudocode ------------------------

#  Input:  List of all data inputs 

#  Assert that we have the correct inputs (at least the first three, four for US)

#  1. Pull out a vector of the poptable IDs and Shapefile IDs
#  2. Join to make sure they match (Sam will do this one) for data_group = "ipums" (see below)
#  3. Make sure the puma_id is included in pop_table (using lookup), 
#  (ignore -->)  contain this WITHIN THE DATA GROUP. Puerto Rico idea 
#  4. Output a table with the specified dimensions (see below)

#  Output (for US) is an updated version of pop_table with:
##  Rows:  All individual places (tracts) in the region to be generated
##  Columns:  place_id (tract ID), puma_id (puma that contains the tract), n_house (# of households)

#  Output (for ipums) is the same as US, but also linking the place_id's in the shapefiles
##  to the place_id's in the pop_table

#  Make sure that the tract IDs match with the shapefile IDs 
##  For data_group = "US", this doesn't really matter
##  For data_group = "ipums", this is important
##  Need to record-link the Place names across shapefiles and other sources



format_data <- function(data_list, data_group) {
    
    if (data_group == "US") {
      #  Assert that we have the correct inputs. (at least the first three, four for US)
      if (!(all(c("shapefiles", "pop_table", "pums", "lookup") %in% names(data_list))))
        stop("Missing a part of data_list")
      
      #  3. Make sure the PUMA's is included in the pop_table (using the lookup table), 
      link_pop_table_to_lookup <- match(data_list$pop_table$place_id, data_list$lookup$place_id)
      data_list$pop_table$puma_id <- data_list$lookup$puma_id[link_pop_table_to_lookup]
      
    } else if (data_group == "ipums") {
      #  Assert that we have the correct inputs. (at least the first three, four for US)
      if (!(all(c("shapefiles", "pop_table", "pums") %in% names(data_list))))
        stop("Missing a part of data_list")
      
    }
  
  return(data_list)
}


