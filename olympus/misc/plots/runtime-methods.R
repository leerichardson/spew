# Lee Richardson 
# October 25, 2017 
# Purpose: Generate Timings for each method in South Dakota, 

devtools::load_all(".")
data("delaware")

places <- delaware$pop_table[which(delaware$pop_table$n_house != 0), "place_id"]
iters <- 1:100
sampling <- c("srs_unif", "srs_roads", "ipf_unif", "ipf_roads")
results <- expand.grid(iters = iters, places = places, sampling = sampling)
results$time <- NA

for (i in iters) {
  for (place in places) {
    for (samp in sampling) {
      results_row <- which(results$iters == i & results$places == place & results$sampling == samp)
      row <- which(delaware$pop_table$place_id == place)  

      if (samp == "srs_unif") {
        runtime <- system.time(
          pop <- spew(pop_table = delaware$pop_table[row, ], 
                                       shapefile = delaware$shapefiles, 
                                       pums_h = delaware$pums$pums_h, 
                                       pums_p = delaware$pums$pums_p, 
                                       schools = delaware$schools, 
                                       workplaces = delaware$workplaces, 
                                       marginals = delaware$marginals, 
                                       sampling_method = "uniform", 
                                       locations_method = "uniform"))
        
      } else if (samp == "srs_roads") {
        runtime <- system.time(
          pop <- spew(pop_table = delaware$pop_table[row, ], 
                                       shapefile = delaware$shapefiles, 
                                       pums_h = delaware$pums$pums_h, 
                                       pums_p = delaware$pums$pums_p, 
                                       schools = delaware$schools, 
                                       workplaces = delaware$workplaces, 
                                       marginals = delaware$marginals, 
                                       sampling_method = "uniform", 
                                       locations_method = "roads"))
        
      } else if (samp == "ipf_unif") {
        runtime <- system.time(
          pop <- spew(pop_table = delaware$pop_table[row, ], 
                                       shapefile = delaware$shapefiles, 
                                       pums_h = delaware$pums$pums_h, 
                                       pums_p = delaware$pums$pums_p, 
                                       schools = delaware$schools, 
                                       workplaces = delaware$workplaces, 
                                       marginals = delaware$marginals, 
                                       sampling_method = "ipf", 
                                       locations_method = "uniform"))
        
      } else if (samp == "ipf_roads") {
        runtime <- system.time(
          pop <- spew(pop_table = delaware$pop_table[row, ], 
                                       shapefile = delaware$shapefiles, 
                                       pums_h = delaware$pums$pums_h, 
                                       pums_p = delaware$pums$pums_p, 
                                       schools = delaware$schools, 
                                       workplaces = delaware$workplaces, 
                                       marginals = delaware$marginals, 
                                       sampling_method = "ipf", 
                                       locations_method = "roads"))
        
      }
      
      cat("Iter: ", i, " Place: ", place, " Samping Method: ", samp, " Runtime: ", as.numeric(runtime[3]), " \n") 
      results[results_row, "time"] <- as.numeric(runtime[3])
    }
  }
}

results <- results[results$iters <= 75, ]
saveRDS(object = results, file = "/home/lee/Dropbox/spew/plots/run_results.rds")

