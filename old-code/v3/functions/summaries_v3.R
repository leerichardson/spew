# Purpose: Loop through each one of the counties and compute a summary
# statistic for a given variable
usa_summary <- function(dir, var) {
  
  # Get a list of all the directories and set up the matrix we will use to
  # store the variable summary statistics by county
  dirs <- list.dirs(dir, recursive = FALSE)
  output_table <- matrix(NA, ncol = 3, nrow = length(dirs))
  colnames(output_table) <- c("county", var, "total")
  count <- 0
  for (dir in dirs) {
    # Get a list of the person level synthetic population files for the given
    # county directory
    output_files <- list.files(dir)
    p_files = output_files[grep("_p.csv", output_files)]
    var_count <- 0
    total <- 0
    
    # Loop through each person level file and compute the total number of
    # people corresponding to the specified variable
    for (pfile in p_files) {
      synth_file <- read.csv(paste0(dir, "/", pfile))
      total <- total + nrow(synth_file)
      target_var <- synth_file[, var]
      tract_var <- length(which(target_var != 1))
      var_count <- var_count + tract_var
    }
    count <- count + 1
    dir_length <- nchar(dir)
    county <- as.numeric(substr(dir, dir_length - 2, dir_length))
    output_table[count, ] <- c(county, var_count, total)
    # print(paste0('Ratio is: ', var_count / total)) print(paste0('Finished
    # county ', count, ' of ', length(dirs)))
  }
  print(output_table)
  total_pop <- sum(output_table[, 3])
  var_ratio <- sum(output_table[, 2])
  print(paste0("Total Population: ", total_pop))
  print(paste0("Var Ratio: ", var_ratio/total_pop))
  write.csv(x = output_table, file = paste0(dir, "county_summaries.csv"))
}

states <- c("KS")
for (state in states) {
  directory <- paste0("/data/shared_group_data/syneco/outputs/west/north_america/united_states/", 
                      state, "/")
  usa_summary(dir = directory, var = "SEX")
} 
