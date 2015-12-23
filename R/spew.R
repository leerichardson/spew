#' Generate synthetic ecosysystems 
#' 
#' @param input_dir character vector specifying the directory containing all of the 
#' input data 
#' @param folders list which specifies the sub-directory for each piece of data 
#' @param data-group character vector, either "US", "ipums", or "none".
#' @param output_dir character vector indicating the directory to write 
#' the synthetic populations 
#' @param paralel logical indicating whether or not the make_data function 
#' should be run in parallel
#' @param sampling_type character vector indicating the type of sampling 
#' to use, defaults to "uniform"
#' 
#' @export
#' @return logical indicating whether or not this run of spew ended successfully 
generate_spew <- function(input_dir, folders, data_group, output_dir, parallel = TRUE, 
                          sampling_type) {
  
  # Given information on our input data, read in everything to memory and 
  # save everything in a list 
  data_list <- read_data(input_dir = input_dir, folders = folders, data_group = data_group)
  
  # Given the data list, make sure everything is formatted correctly 
  formatted_data <- format_data(data_list = data_list, data_group = data_group)
  
  # Use the formatted data to generate synthetic populations 
  make_data(pop_table = formatted_data$pop_table, shapefile = formatted_data$shapefiles, 
            pums_h = formatted_data$pums$pums_h, pums_p = formatted_data$pums$pums_p, 
            parallel = parallel, sampling_type = sampling_type, output_dir = output_dir)
  
  return(TRUE)
}
