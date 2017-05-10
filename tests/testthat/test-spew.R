context("SPEW")


test_that("Parallel Backends", {
  # Skip the parallel backend tests if Rmpi not installed 
  skip_if_not_installed("Rmpi")

  data(sd_data)
  data(uruguay_format)
  library(sp)
  library(rgeos)
  library(data.table)
  library(parallel)
  library(Rmpi)
  library(foreach)
  library(doParallel)
  
  
  # Open a file to store the outputs of this test-run 
  dir.create("tmp")
  sink("test_output.txt")
  
  # Check that the three parallel types work -------------------
  places <- 1:2  
  sock <- spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
               schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL,
               pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
               base_dir = "tmp/", convert_count = FALSE,  parallel_type = "SOCK",
               sampling_method = "uniform", locations_method = "uniform", 
               outfile_loc = "/dev/null")  
  
#   mc <- spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
#              schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL,
#              pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
#              base_dir = "tmp/", parallel_type = "MC", convert_count = FALSE, 
#              sampling_method = "uniform", locations_method = "uniform", 
#              outfile_loc = "/dev/null")
# expect_true(sock[[1]]$total_households == mc[[1]]$total_households) 
  
  mpi <- spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
            schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL,
            pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p,
            base_dir = "tmp/", parallel_type = "MPI", convert_count = FALSE, 
            sampling_method = "uniform", locations_method = "uniform", 
            outfile_loc = "/dev/null")
  expect_true(sock[[1]]$total_households == mpi[[1]]$total_households)
  
  sink()
  unlink("test_output.txt")
  unlink("tmp/", recursive = TRUE)  
})

test_that("SPEW algorithm runs as expected", {
  data(sd_data)
  data(uruguay_format)
  library(sp)
  library(rgeos)
  library(data.table)

  # Make sure the 0 household places are caught
  test_ind <- 100
  sd_data$pop_table[test_ind, "n_house"] <- 0
  expect_output(spew_place(test_ind, sd_data$pop_table, sd_data$shapefiles, 
                           sd_data$pums$pums_h, sd_data$pums$pums_p, 
                           schools = sd_data$schools, workplaces = sd_data$workplaces,
                           sampling_method = "uniform", locations_method = "uniform", 
                           output_dir = "~/Desktop/46", convert_count = FALSE), 
                "Place has 0 Households!")

  # Verify SPEW algorithms runs as expected ---------
  dir.create("tmp")
  sink("test_output.txt")
  
  # Run SPEW on South Dakota 
  places <- 1:4
  regular_md <- spew(pop_table = sd_data$pop_table[places, ], shapefile = sd_data$shapefiles$shapefile,
                                 schools = sd_data$schools, workplaces = sd_data$workplaces, marginals = NULL, 
                                 pums_h = sd_data$pums$pums_h, pums_p = sd_data$pums$pums_p, parallel_type = "SEQ",
                                 base_dir = "tmp/", convert_count = FALSE, sampling_method = "uniform", 
                                 locations_method = "uniform")

  # Verify the environments are there and the correct written CSV's 
  expect_true(dir.exists("tmp/output/environments"))
  expect_true(file.exists("tmp/output/environments//private_schools.csv"))

  # Run one region of Uruguay
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

  # Remove all of the temporary outputs we used for testing 
  sink()
  unlink("test_output.txt")
  unlink("tmp/", recursive = TRUE)  
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
