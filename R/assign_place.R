#' Assign a place to a person
#'
#' @param people data frame of synthetic people produced by SPEW
#' @param data_list of the data and identifying name
#' @return column corresponding to the people of the place assignment
assign_place <- function(people, data_list){
    
    col <- do.call(paste0("assign_", data_list$name), args=list(people=people, data_list$data))
    return(col)
}
