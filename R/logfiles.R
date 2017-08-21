#' Parse a SPEW Log-file to into an appropriate column
#'
#' @param logfile character vector of a SPEW log-file
#' @param name character with the name of the column to
#' extract from the SPEW log-file
#' @param character default is "numeric". Determines whether
#' or not the final lgogfile should be converted to a numeric.
create_column <- function(logfile, name, type = "numeric") {
  # Subset the rows and remove the excess from
  # the logfiles
  logfile <- get_rows(logfile, name)
  logfile <- remove_excess(logfile)
  logfile <- gsub(" ", "", logfile)
  
  # Split the clean rows by the colon separating them
  # and return the column with everything to the right
  # of the column
  logfile <- strsplit(x = logfile, split = ":")
  after_colon <- unlist(lapply(logfile, function(x) x[length(x)]))
  
  # Remove the error colums IF there was an error in the log-file row
  if (length(grep("Place Name", name)) != 1) {
    after_colon <- as.numeric(after_colon)
  }
  
  return(after_colon)
}

#' Convert a SPEW Logfile into a data-frame
#'
#' @param logfile character with the file-name
#' @param columns character vector indicating the
#' names of the columns we are extracting from the
#' log-file
spewlog_to_df <- function(logfile, columns) {
  # Loop through each one of the Columns
  # and make each one a data-frame columns
  for (col in columns) {
    # Extract column info from log-file
    current_col <- create_column(logfile, col)
    
    # If this is the first column, initialize the
    # data-frame to the appropriate length
    if (col == columns[1]) {
      spewlog_df <- data.frame(matrix(NA, nrow = length(current_col), ncol = length(columns)))
      spewlog_df[, 1] <- current_col
    } else {
      # Verify that the column is the same length as the data-frame.
      # If not, just insert all NA's for this specfic column
      current_col <- verify_column(column = current_col, df_size = nrow(spewlog_df))
      
      # Add the column to the appropriate index in the data-frame
      spewlog_df[, which(col == columns)] <- current_col
    }
  }
  colnames(spewlog_df) <- gsub(":", "", columns)
  return(spewlog_df)
}

#' Verify the column is the correct size
#'
#' @param column to be verified
#' @param data-frame size
verify_column <- function(column, df_size) {
  # Check to see whether it's the same length.
  # If not, convert the column to appropriate sized NA's
  if (length(column) != df_size) {
    column <- rep(NA, df_size)
  }
  return(column)
}

#' Check to see if a SPEW log-file is complete
#'
#' @param logfile character vecor of a SPEW Log-file
#' @return character string indicating the status of the
#' SPEW log-file
check_logfile <- function(logfile) {
  # Verify the log-file made it to the
  # last line of SPEW
  pops <- get_rows(logfile, "SPEW Runs in:" )
  if (length(pops) == 1) {
    pop_result <- TRUE
  } else {
    pop_result <- FALSE
  }
  
  if (pop_result) {
    return("Complete!")
  } else {
    return("Incomplete!")
  }
}

#' Extract data-group from location name
#'
#' @param location name character vector
get_data_group <- function(location_name) {
  # Make sure this isn't the custom data group
  # which right now only includes Canada
  if (location_name == "canada") {
    data_group <- "custom"
    return(data_group)
  }
  
  # If as.numeric gives a numeric, it's the USA
  # ID. If not, it's IPUMS
  location_name <- as.numeric(location_name)
  if (is.na(location_name)) {
    data_group <- "ipums"
  } else {
    data_group = "us"
  }
  
  return(data_group)
}

#' Extract the total run-time from a SPEW log-file
#'
#' @param SPEW log-file
get_total_time <- function(logfile) {
  total_line <- get_rows(logfile, "SPEW Runs in:")
  total_numbers <- number_from_char(total_line)
  total_time <- paste0(total_numbers[2], ".", total_numbers[3])
  return(total_time)
}
#' Parse a SPEW Log-file to into an appropriate column
#'
#' @param logfile character vector of a SPEW log-file
#' @param name character with the name of the column to
#' extract from the SPEW log-file
#' @param character default is "numeric". Determines whether
#' or not the final lgogfile should be converted to a numeric.
create_column <- function(logfile, name, type = "numeric") {
  # Subset the rows and remove the excess from
  # the logfiles
  logfile <- get_rows(logfile, name)
  logfile <- remove_excess(logfile)
  logfile <- gsub(" ", "", logfile)
  
  # Split the clean rows by the colon separating them
  # and return the column with everything to the right
  # of the column
  logfile <- strsplit(x = logfile, split = ":")
  after_colon <- unlist(lapply(logfile, function(x) x[length(x)]))
  
  # Remove the error colums IF there was an error in the log-file row
  if (length(grep("Place Name", name)) != 1) {
    after_colon <- as.numeric(after_colon)
  }
  
  return(after_colon)
}

#' Convert a SPEW Logfile into a data-frame
#'
#' @param logfile character with the file-name
#' @param columns character vector indicating the
#' names of the columns we are extracting from the
#' log-file
spewlog_to_df <- function(logfile, columns) {
  # Loop through each one of the Columns
  # and make each one a data-frame columns
  for (col in columns) {
    # Extract column info from log-file
    current_col <- create_column(logfile, col)
    
    # If this is the first column, initialize the
    # data-frame to the appropriate length
    if (col == columns[1]) {
      spewlog_df <- data.frame(matrix(NA, nrow = length(current_col), ncol = length(columns)))
      spewlog_df[, 1] <- current_col
    } else {
      # Verify that the column is the same length as the data-frame.
      # If not, just insert all NA's for this specfic column
      current_col <- verify_column(column = current_col, df_size = nrow(spewlog_df))
      
      # Add the column to the appropriate index in the data-frame
      spewlog_df[, which(col == columns)] <- current_col
    }
  }
  colnames(spewlog_df) <- gsub(":", "", columns)
  return(spewlog_df)
}

#' Verify the column is the correct size
#'
#' @param column to be verified
#' @param data-frame size
verify_column <- function(column, df_size) {
  # Check to see whether it's the same length.
  # If not, convert the column to appropriate sized NA's
  if (length(column) != df_size) {
    column <- rep(NA, df_size)
  }
  return(column)
}

#' Check to see if a SPEW log-file is complete
#'
#' @param logfile character vecor of a SPEW Log-file
#' @return character string indicating the status of the
#' SPEW log-file
check_logfile <- function(logfile) {
  # Verify the log-file made it to the
  # last line of SPEW
  pops <- get_rows(logfile, "SPEW Runs in:" )
  if (length(pops) == 1) {
    pop_result <- TRUE
  } else {
    pop_result <- FALSE
  }
  
  if (pop_result) {
    return("Complete!")
  } else {
    return("Incomplete!")
  }
}

#' Extract data-group from location name
#'
#' @param location name character vector
get_data_group <- function(location_name) {
  # Make sure this isn't the custom data group
  # which right now only includes Canada
  if (location_name == "canada") {
    data_group <- "custom"
    return(data_group)
  }
  
  # If as.numeric gives a numeric, it's the USA
  # ID. If not, it's IPUMS
  location_name <- as.numeric(location_name)
  if (is.na(location_name)) {
    data_group <- "ipums"
  } else {
    data_group = "us"
  }
  
  return(data_group)
}

#' Extract the total run-time from a SPEW log-file
#'
#' @param SPEW log-file
get_total_time <- function(logfile) {
  total_line <- get_rows(logfile, "SPEW Runs in:")
  total_numbers <- number_from_char(total_line)
  total_time <- paste0(total_numbers[2], ".", total_numbers[3])
  return(total_time)
}


#' Extract rows with a certain character
#'
#' @param dat character vector
#' @param char character which specifies the
#' sequence we are looking for
#' @return output which is the subsetted rows
#' of the initial character vector (logfile)
get_rows <- function(dat, char) {
  row_index <- grep(char, dat)
  output <- dat[row_index]
  return(output)
}
