context("Make functions")

test_that("Sampling functions", {
  
  # Load in the formatted data 
  data(sd_data)
  data(uruguay_format)
  library(stringdist)
  library(sp)
  
  # Sample locations --------------
  multiple_polygons <- sample_locations(method = "uniform", place_id = 46027965700, n_house = 100, 
                                        shapefile = sd_data$shapefiles)
  expect_equal(is.null(multiple_polygons), FALSE)
  
  num_samples <- floor(runif(1, min = 1, max = 200))
  rand_row <- floor(runif(1, min = 1, max = nrow(sd_data$pop_table)))  
  single_polygon <- sample_locations(method = "uniform", place_id = sd_data$pop_table[rand_row, "place_id"], 
                                    n_house = num_samples, shapefile = sd_data$shapefiles)
  expect_equal(length(single_polygon), num_samples)
  
  # Verify the ipums shapefiles work as well using Uruguay data 
  place_names <- uruguay_format$shapefiles$place_id
  for (place in place_names) {
    samp <- sample_locations(method = "uniform", place, num_samples, uruguay_format$shapefiles)
    expect_equal(length(samp), num_samples)
  }
  
  # Make sure the 0 household places are caught
  test_ind <- 100
  sd_data$pop_table[test_ind, "n_house"] <- 0
  expect_output(make_place(test_ind, sd_data$pop_table, sd_data$shapefiles, 
                         sd_data$pums$pums_h, sd_data$pums$pums_p, 
                         schools = sd_data$schools, workplaces = sd_data$workplaces,
                         sampling_method = "uniform", locations_method = "uniform", 
                         output_dir = "~/Desktop/46", convert_count = FALSE), 
                         "Place has 0 Households!")
  
  # Test that the Parallel version is quicker ----------------
  library(doSNOW)
  library(foreach)
  
  # Open a file to store the outputs of this test-run 
  dir.create("tmp")
  sink("test_output.txt")
  
  # Make sure the parallel runs faster than the regular ---------
  places <- 1:4
  regular_md <- system.time(make_data(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles,
                                      schools = sd_data$schools, workplaces = sd_data$workplaces,
                                      pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
                                      output_dir = "tmp/", parallel = FALSE, convert_count = FALSE, 
                                      sampling_method = "uniform", locations_method = "uniform"))
  
  parallel_md <- system.time(make_data(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles,
                                       schools = sd_data$schools, workplaces = sd_data$workplaces,
                                       pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
                                       output_dir = "tmp/", parallel = TRUE, convert_count = FALSE, 
                                       sampling_method = "uniform", locations_method = "uniform"))

  expect_equal(as.logical(parallel_md[1] < regular_md[1]), TRUE)

  # Test the Serial Synth and convert count functions ---------------
  uruguay_region <- make_place(index = 1, pop_table = uruguay_format$pop_table, 
                               shapefile = uruguay_format$shapefiles, pums_h = uruguay_format$pums$pums_h, 
                               schools = uruguay_format$schools, workplaces = uruguay_format$workplaces,
                               pums_p = uruguay_format$pums$pums_p, sampling_method = "uniform", 
                               locations_method = "uniform", output_dir = "tmp/", convert_count = TRUE)
  
  synth_pums_h <- read.csv("tmp/output_858002/eco/household_artigas.csv")
  synth_pums_p <- read.csv("tmp/output_858002/eco/people_artigas.csv")
  
  # Verify the printing of region lists works as expected 
  expect_true(print_region_list(uruguay_region))  
  
  # Make sure the synthetic serial I.D. makes it in, and that 
  # there are less of these than the original, in case of duplicated columns
  expect_equal("SYNTHETIC_HID" %in% names(synth_pums_h), TRUE)
  expect_equal("SYNTHETIC_PID" %in% names(synth_pums_p), TRUE)
  expect_equal(max(table(synth_pums_p$SYNTHETIC_SERIAL)) < max(table(synth_pums_p$SERIALNO)), TRUE)
  
  # Make sure the convert_count functions work...
  original_nhouse <- uruguay_format$pop_table[1, "n_house"]
  expect_equal(nrow(synth_pums_h) == original_nhouse, FALSE)
  expect_equal(abs( (nrow(synth_pums_p) / original_nhouse) - 1) < .2, TRUE)
  
  # Testing that the schools an workplace functions 
  # are integrated into make properly -------------
    
  
  # Test that the new sampling methods are integrated 
  # correctly ---------------------------------------
  
  
  # Remove all of the temporary outputs we used for testing 
  sink()
  unlink("test_output.txt")
  unlink("tmp/", recursive = TRUE)
})

