#' Generate synthetic ecosysystems 
#' 
#' @param input_dir character vector specifying the directory containing all of the 
#' input data 
#' @param folders list which specifies the sub-directory for each piece of data 
#' @param data-group character vector, either "US", "ipums", or "none".
#' @param output_dir character vector indicating the directory to write 
#' the synthetic populations 
#' @param parallel logical indicating whether or not the make_data function 
#' should be run in parallel
#' @param sampling_method character vector indicating the type of sampling 
#' method to use, defaults to "uniform"
#' @param locations_method character vector indicating the type of location 
#' sampling to use, defaults to "uniform", can also be "roads". 
#' @param convert_count logical meant to indicate if we are going to convert 
#' population totals to househole counts
#' @param vars list with two components: household and person. This specifies 
#' which variables to include in the corresponding PUMS data-set  
#' @param make_plots logical indicating whether we should make maps of 
#' the synthetic households.  
#' @export
#' @return logical indicating whether or not this run of spew ended successfully 
generate_spew <- function(input_dir, folders, data_group, output_dir, parallel = TRUE, 
                          sampling_method = "uniform", locations_method = "uniform", 
                          convert_count = FALSE, vars = list(household = NA, person = NA)) {
  
  # Start timing the function 
  start_time <- Sys.time()
  
  # Given information on our input data, read in everything to memory and 
  # save everything in a list 
  data_list <- read_data(input_dir = input_dir, folders = folders, 
                         data_group = data_group, vars)
  
  # Given the data list, make sure everything is formatted correctly 
  formatted_data <- format_data(data_list = data_list, data_group = data_group)
  
  # Use the formatted data to generate synthetic populations 
  make_data(pop_table = formatted_data$pop_table, shapefile = formatted_data$shapefiles, 
            pums_h = formatted_data$pums$pums_h, pums_p = formatted_data$pums$pums_p, 
            schools = formatted_data$schools, workplaces = formatted_data$workplaces, 
            parallel = parallel, output_dir = output_dir, sampling_method = sampling_method, 
            locations_method = locations_method, convert_count = convert_count)

  # Print out the overall run-time of SPEW!
  overall_time <- difftime(Sys.time(), start_time,units = "secs")
  overall_time <- round(overall_time, digits = 2)
  
  statement <- paste0("SPEW Runs in: ", overall_time)
  print(statement)
  
  return(TRUE)
}
