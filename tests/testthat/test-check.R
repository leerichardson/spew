context("Check Functions")

test_that("data has all the required pieces", {
  
  # Pop table, shapefile, and PUMS have the 
  # necessary components. AND, the components match 
  # with one another --------------------
  
  # Uruguay ipums data 
  data(uruguay_format) 
  
  expect_equal(check_pop_table(uruguay_format$pop_table), "Pop table is ready!")
  expect_equal(check_shapefile(shapefile = uruguay_format$shapefiles), "Shapefile is ready!")
  expect_equal(check_pums(pums = uruguay_format$pums), "Pums is ready!")
  
  uruguay_count_ids <- uruguay_format$pop_table$place_id
  uruguay_count_pumas <- uruguay_format$pop_table$puma_id
  uruguay_shape_ids <- uruguay_format$shapefiles$place_id
  uruguay_pums_pumas <- uruguay_format$pums$pums_h$puma_id  
  
  expect_equal(check_place_ids(id1 = uruguay_count_ids, id2 = uruguay_shape_ids), "Place ids match!")
  expect_equal(check_puma_ids(uruguay_count_pumas, uruguay_pums_pumas), "Puma ids match!")
  
  # American South Dakota Data 
  data(sd_data)
  
  expect_match(check_pop_table(sd_data$pop_table), "Pop table is ready!")
  expect_equal(check_shapefile(sd_data$shapefiles), "Shapefile is ready!")
  expect_equal(check_pums(sd_data$pums), "Pums is ready!")
  
  sd_count_ids <- sd_data$pop_table$place_id
  sd_count_pumas <- sd_data$pop_table$puma_id
  sd_shape_ids <- sd_data$shapefiles$place_id
  sd_pums_pumas <- sd_data$pums$pums_h$puma_id  
  
  expect_equal(check_place_ids(id1 = sd_count_ids, id2 = sd_shape_ids), "Place ids match!")
  expect_equal(check_puma_ids(sd_count_pumas, sd_pums_pumas), "Puma ids match!")
  
})

test_that("data source locations match with one another", {
  
})