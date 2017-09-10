context("Check Functions")

test_that("data has all the required pieces", {
  # Pop table, shapefile, and PUMS have the 
  # necessary components. AND, the components match 
  # with one another --------------------
  
  # Uruguay ipums data 
  data("uruguay") 
  
  expect_equal(check_pop_table(uruguay$pop_table), "Pop table is ready!")
  expect_equal(check_shapefile(shapefile = uruguay$shapefiles), "Shapefile is ready!")
  expect_equal(check_pums(pums = uruguay$pums), "Pums is ready!")
  
  uruguay_count_ids <- uruguay$pop_table$place_id
  uruguay_count_pumas <- uruguay$pop_table$puma_id
  uruguay_shape_ids <- uruguay$shapefiles$place_id
  uruguay_pums_pumas <- uruguay$pums$pums_h$puma_id  
  
  expect_equal(check_place_ids(id1 = uruguay_count_ids, id2 = uruguay_shape_ids), "Place ids match!")
  expect_equal(check_puma_ids(uruguay_count_pumas, uruguay_pums_pumas), "Puma ids match!")
  
  # American South Dakota Data 
  data(delaware)
  
  expect_match(check_pop_table(delaware$pop_table), "Pop table is ready!")
  expect_equal(check_shapefile(delaware$shapefiles), "Shapefile is ready!")
  expect_equal(check_pums(delaware$pums), "Pums is ready!")
  
  del_count_ids <- delaware$pop_table$place_id
  del_count_pumas <- delaware$pop_table$puma_id
  del_shape_ids <- delaware$shapefiles$shapefile$place_id
  del_pums_pumas <- delaware$pums$pums_h$puma_id  
  
  expect_equal(check_place_ids(id1 = del_count_ids, id2 = del_shape_ids), "Place ids match!")
  expect_equal(check_puma_ids(del_count_pumas, del_pums_pumas), "Puma ids match!")
})
