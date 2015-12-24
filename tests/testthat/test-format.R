context("Format functions")

test_that("United States Formatting", { 
  data(sd_data)
  
  # Check to make sure the merge is of the pop_table 
  # and looking table works and is using the same class 
  sd_data$pop_table <- sd_data$pop_table[, 1:2]  
  fd <- format_data(data_list = sd_data, data_group = "US")
  merged_puma <- fd$pop_table$puma_id
  expect_equal(any(is.na(merged_puma)), FALSE)
  
}) 
