context("Format functions")

test_that("United States formatting", { 
  # Write all format output to a file 
  data(delaware)
  
  # Check to make sure the merge is of the pop_table 
  # and looking table works and is using the same class 
  delaware$pop_table <- delaware$pop_table[, 1:2]  
  fd <- format_data(data_list = delaware, data_group = "US", verbose = FALSE)
  merged_puma <- fd$pop_table$puma_id
  expect_equal(any(is.na(merged_puma)), FALSE)
}) 

test_that("ipums formatting", { 
  skip_if_not_installed("stringdist")
  
  # Write all format output to a file 
  uruguay_path <- system.file("extdata/ury", package = "spew")
  uruguay_raw <- read_data(base_dir = uruguay_path, 
                       data_group = "ipums", 
                       folders = list(pop_table = "counts", 
                                      pums = "pums", 
                                      shapefiles = "shapefiles"))
  library(stringdist)
  
  # Check that we are getting the accurate level 
  shape_names <- uruguay_raw$shapefiles$place_id
  level <- get_level(shape_names, uruguay_raw$pop_table)
  expect_equal(level, "level2")
  
  level_indices <- which(uruguay_raw$pop_table$level == level)
  count_names <- uruguay_raw$pop_table$place_id[level_indices]
  shape_indices <- get_shapefile_indices(shape_names, count_names)

  # Make sure the formatted data is doing the right thing 
  uruguay_format <- format_data(data_list = uruguay_raw, data_group = "ipums", verbose = FALSE)
  expect_equal(nrow(uruguay_format$pop_table) == 19, TRUE)
  expect_equal(all(uruguay_format$pop_table$place_id == uruguay_format$pop_table$place_id), TRUE)  

  # Make sure allocate_count is working as expected 
  pseudo_counts <- floor(seq(1, 1000, length = 30))
  new_counts <- allocate_count(counts = pseudo_counts, count_id = 4)
  expect_equal(length(new_counts) < length(pseudo_counts), TRUE)
  expect_equal(new_counts[1] > pseudo_counts[1], TRUE)
  
  # Verify that we can combine counts and remove rows 
  pt_remove <- remove_count(pop_table = uruguay_format$pop_table, place = "rocha")
  expect_equal(nrow(pt_remove) == 18, TRUE)
})
