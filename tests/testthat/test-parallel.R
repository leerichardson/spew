context("Parallel")

test_that("Parallel Backends", {
  # Skip this test formally, but use this file to debug for olympus ---
  testthat::skip(message = "Skipping parallel tests in formal suite")
  
  data(delaware)
  library(sp)
  library(rgeos)
  library(data.table)
  library(parallel)
  library(Rmpi)
  library(foreach)
  library(doParallel)
  
  # Check that the three parallel types work -------------------
  places <- 1:2
  delaware_seq <- spew(pop_table = delaware$pop_table[places, ], 
                        shapefile = delaware$shapefiles,
                        pums_h = delaware$pums$pums_h, 
                        pums_p = delaware$pums$pums_p, 
                        verbose = FALSE, run_type = "SEQ")

  delaware_sock <- spew(pop_table = delaware$pop_table[places, ], 
                         shapefile = delaware$shapefiles,
                         pums_h = delaware$pums$pums_h, 
                         pums_p = delaware$pums$pums_p, 
                         verbose = FALSE, run_type = "SOCK", 
                         outfile_loc = "/dev/null")
  
  delaware_mc <- spew(pop_table = delaware$pop_table[places, ], 
                         shapefile = delaware$shapefiles,
                         pums_h = delaware$pums$pums_h, 
                         pums_p = delaware$pums$pums_p, 
                         verbose = FALSE, run_type = "MC")

  expect_equal(delaware_seq[[1]]$place_id, delaware_sock[[1]]$place_id)
  expect_equal(nrow(delaware_seq[[1]]$households), nrow(delaware_sock[[1]]$households))
  
  expect_equal(delaware_seq[[1]]$place_id, delaware_mc[[1]]$place_id)
  expect_equal(nrow(delaware_seq[[1]]$households), nrow(delaware_mc[[1]]$households))  
  
  delaware_mpi <- spew(pop_table = delaware$pop_table[places, ], 
                       shapefile = delaware$shapefiles,
                       pums_h = delaware$pums$pums_h, 
                       pums_p = delaware$pums$pums_p, 
                       verbose = TRUE, run_type = "MPI")
})
