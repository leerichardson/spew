context("Make functions")

test_that("Sampling functions", {
  
  # Load in the formatted data 
  data(sd_data)
  
  # Sample locations --------------
  multiple_polygons <- sample_locations(place_id = 46027965700, n_house = 110, 
                                        shapefile = sd_data$shapefiles)
  expect_equal(is.null(multiple_polygons), FALSE)
  
  num_samples <- floor(runif(1, min = 1, max = 200))
  rand_row <- floor(runif(1, min = 1, max = nrow(sd_data$pop_table)))  
  single_polygon <-sample_locations(place_id = sd_data$pop_table[rand_row, "place_id"], 
                                    n_house = num_samples, shapefile = sd_data$shapefiles)
  expect_equal(length(single_polygon), num_samples)
  
  # Make sure the 0 household places are caught
  test_ind <- 100
  sd_data$pop_table[test_ind, "n_house"] <- 0
  
  expect_output(make_place(test_ind, sd_data$pop_table, sd_data$shapefiles, 
                         sd_data$pums$pums_h, sd_data$pums$pums_p, 
                         sampling_type = "uniform", output_dir = "/home/lee", convert_count = FALSE), 
                          "Place has 0 Households!")
  
  # Test that the Parallel version is quicker ----------------
  library(doParallel)
  library(foreach)
  
  places <- 1:4 
  sink("test_output.txt")
  
  regular_md <- system.time(make_data(sd_data$pop_table[places, ], sd_data$shapefiles, 
                                      sd_data$pums$pums_h, sd_data$pums$pums_p, 
                                      convert_count = FALSE))
  parallel_md <- system.time(make_data(sd_data$pop_table[places, ], sd_data$shapefiles, 
                                      sd_data$pums$pums_h, sd_data$pums$pums_p, 
                                      parallel = TRUE, convert_count = FALSE))
  
  sink()
  file.remove("test_output.txt")
  
  expect_equal(as.logical(parallel_md[1] < regular_md[1]), TRUE)

  # Test the Serial Synth and convert count functions ---------------
  data(uruguay_data)
  library(stringdist)
  uruguay_format <- format_data(data_list = uruguay_data, 
                                data_group = "ipums")
  
  dir.create("tmp")
  make_place(index = 1, pop_table = uruguay_format$pop_table, 
             shapefile = uruguay_format$shapefiles, pums_h = uruguay_format$pums$pums_h, 
             pums_p = uruguay_format$pums$pums_p, sampling_type = "uniform", 
             output_dir = "tmp/", convert_count = TRUE)
  
  synth_pums_h <- read.csv("tmp/household_Artigas.csv")
  synth_pums_p <- read.csv("tmp/people_Artigas.csv")
  
  # Make sure the synthetic serial I.D. makes it in, and that 
  # there are less of these than the original, in case of duplicated columns
  expect_equal("SYNTHETIC_SERIAL" %in% names(synth_pums_h), TRUE)
  expect_equal(max(table(synth_pums_p$SYNTHETIC_SERIAL)) < max(table(synth_pums_p$SERIALNO)), TRUE)
  
  # Make sure the convert_count functions work...
  original_nhouse <- uruguay_format$pop_table[1, "n_house"]
  expect_equal(nrow(synth_pums_h) == original_nhouse, FALSE)
  expect_equal(abs( (nrow(synth_pums_p) / original_nhouse) - 1) < .2, TRUE)
  
  unlink("tmp/", recursive = TRUE)
  
  # Testing the schools function 
  
})
