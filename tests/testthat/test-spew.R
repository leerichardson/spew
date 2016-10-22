context("SPEW")

test_that("SPEW algorithm runs as expected", {
  data(sd_data)
  data(uruguay_format)
  
  library(stringdist)
  library(sp)
  library(data.table)
  
  library(Rmpi)
  library(doSNOW)
  library(doMC)
  library(foreach)
  
  # Sample locations --------------
  multiple_polygons <- sample_locations(method = "uniform", place_id = 46027965700, n_house = 100, 
                                        shapefile = sd_data$shapefiles$shapefile)
  expect_equal(is.null(multiple_polygons), FALSE)
  
  num_samples <- floor(runif(1, min = 1, max = 200))
  rand_row <- floor(runif(1, min = 1, max = nrow(sd_data$pop_table)))  
  single_polygon <- sample_locations(method = "uniform", place_id = sd_data$pop_table[rand_row, "place_id"], 
                                     n_house = num_samples, shapefile = sd_data$shapefiles$shapefile)
  expect_equal(length(single_polygon), num_samples)
  
  # Verify the ipums shapefiles work as well using uruguay data 
  place_names <- uruguay_format$shapefiles$place_id
  for (place in place_names[1:6]) {
    samp <- sample_locations(method = "uniform", place, num_samples, uruguay_format$shapefiles)
    expect_equal(length(samp), num_samples)
  }
  
  # Make sure the 0 household places are caught
  test_ind <- 100
  sd_data$pop_table[test_ind, "n_house"] <- 0
  expect_output(spew_place(test_ind, sd_data$pop_table, sd_data$shapefiles, 
                           sd_data$pums$pums_h, sd_data$pums$pums_p, 
                           schools = sd_data$schools, workplaces = sd_data$workplaces,
                           sampling_method = "uniform", locations_method = "uniform", 
                           output_dir = "~/Desktop/46", convert_count = FALSE), 
                "Place has 0 Households!")
  
  
  # Open a file to store the outputs of this test-run 
  dir.create("tmp")
  sink("test_output.txt")
  
  # Check that the three parallel types work -------------------
  places <- 1:2  
  sock <- spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
               schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL,
               pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
               base_dir = "tmp/", parallel = TRUE, convert_count = FALSE, 
               sampling_method = "uniform", locations_method = "uniform", 
               parallel_type = "SOCK", outfile_loc = "/dev/null")
  
  mpi <- spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
              schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL,
              pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
              base_dir = "tmp/", parallel = TRUE, convert_count = FALSE, 
              sampling_method = "uniform", locations_method = "uniform", 
              parallel_type = "MPI", outfile_loc = "/dev/null")  
  
#   mc <- spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
#              schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL,
#              pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
#              base_dir = "tmp/", parallel = TRUE, convert_count = FALSE, 
#              sampling_method = "uniform", locations_method = "uniform", 
#              parallel_type = "MC", outfile_loc = "/dev/null")  
#   expect_true(sock[[1]]$total_households == mc[[1]]$total_households) 

  expect_true(sock[[1]]$total_households == mpi[[1]]$total_households)
  
  # Make sure the parallel runs faster than the regular ---------
  places <- 1:4
  regular_md <- system.time(spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
                                 schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL, 
                                 pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
                                 base_dir = "tmp/", parallel = FALSE, convert_count = FALSE, 
                                 sampling_method = "uniform", locations_method = "uniform"))
  
  parallel_md <- system.time(spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
                                  schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL,
                                  pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
                                  base_dir = "tmp/", parallel = TRUE, convert_count = FALSE, 
                                  sampling_method = "uniform", locations_method = "uniform", parallel_type = "SOCK", 
                                  outfile_loc = "/dev/null"))  
  expect_true(as.logical(parallel_md[1] < regular_md[1]))
  
  # Test the Serial Synth and convert count functions ---------------
  uruguay_region <- spew_place(index = 1, pop_table = uruguay_format$pop_table, 
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
  expect_true("SYNTHETIC_HID" %in% names(synth_pums_h))
  expect_true("SYNTHETIC_PID" %in% names(synth_pums_p))
  expect_true(max(table(synth_pums_p$SYNTHETIC_PID)) < max(table(synth_pums_p$SERIALNO)))
  
  # Make sure the convert_count functions work...
  original_nhouse <- uruguay_format$pop_table[1, "n_house"]
  expect_equal(nrow(synth_pums_h) == original_nhouse, FALSE)
  expect_equal(abs( (nrow(synth_pums_p) / original_nhouse) - 1) < .2, TRUE)
  
  # Create a large region iteratively 
  #   uruguay_format$pop_table[3, "n_house"] <- 4000000  
  #   uruguay_large <- spew_place(index = 3, pop_table = uruguay_format$pop_table, 
  #                                shapefile = uruguay_format$shapefiles, pums_h = uruguay_format$pums$pums_h, 
  #                                schools = uruguay_format$schools, workplaces = uruguay_format$workplaces,
  #                                pums_p = uruguay_format$pums$pums_p, sampling_method = "uniform", 
  #                                locations_method = "uniform", output_dir = "tmp/", convert_count = TRUE)
  #   
  #   large_tst <- fread("tmp/output_858004/eco/people_cerrolargo.csv")
  #   dif <- abs(nrow(large_tst) - 4000000)
  #   expect_true(dif < 500000)
  
  # Remove all of the temporary outputs we used for testing 
  sink()
  unlink("test_output.txt")
  unlink("tmp/", recursive = TRUE)  

  # Test that we can partition large population tables 
  # that cause the parallel runs to fails
  expect_equal(partition_pt(222, 100), c(1, 101, 201, 222))
  expect_equal(partition_pt(222, 200), c(1, 201, 222))
  expect_equal(partition_pt(1000, 100), c(1, 101, 201, 301, 401, 501, 601, 701, 801, 901, 1000))
    
})

test_that("SPEW wrapper runs as expected", {
#   Load in the data and Libraries 
#   library(foreach)
#   library(doSNOW)
#   library(sp)
#   library(maptools)
#   library(stringdist)
#   library(mipfp)
#   
#   data(sd_data)
#   data(uruguay_format)
#   data(delaware)
#   
#   # Lee: If you run these, remember to change the output directory!
#   # They take too long to put in the automated tests, but they're 
#   # a good sanity check to make sure that everything still links up 
# 
#   # Test spew wrapper function on delaware----------------
#   us_vars <- list(household = c("RT", "TYPE", "SERIALNO", "PUMA", "REGION", "ST", "HINCP", "NP"), 
#                   person = c("RT", "SERIALNO", "PUMA", "ST", "SEX", "AGEP",  "SCH", "SCHG", "RELP", 
#                              "HISP", "ESR", "PINCP", "NATIVITY", "OCCP", "POBP", "RAC1P"))
#   delaware_folders <- list(pop_table = "counts/natstat/2010/tract", 
#                            pums = "pums/natstat/2013/puma", 
#                            shapefiles = "shapefiles/natstat/2010/tract", 
#                            roads = "roads/natstat/2010/county", 
#                            schools = "schools/natstat/2013/county", 
#                            lookup = "lookup/natstat/2010/tract", 
#                            workplaces = "workplaces/natstat/2009/county", 
#                            marginals = "marginals/natstat/2014/tract")
# 
#   call_spew(base_dir = "data-raw/10", 
#             folders = delaware_folders, 
#             data_group = "US", 
#             parallel = TRUE, 
#             sampling_method = "ipf", 
#             locations_method = "roads", 
#             convert_count = FALSE, 
#             vars = us_vars)
})

