# Lee Richardson
# September 9, 2017 
# Purpose: Reduce the size of external data for submission to CRAN 

# Delaware -------------------------------------------------

# Lookup ---
lookup <- read.csv("inst/extdata/10/input/lookup/natstat/2010/tract/lookup10.csv")
delaware <- lookup[which(lookup$STATEFP == 10), c(2, 3, 4, 5)]
write.csv(x = delaware, file = "inst/extdata/10/input/lookup/natstat/2010/tract/lookup10.csv")

# PUMS ---
household_vars <- c("RT", "TYPE", "SERIALNO",  "PUMA", "HINCP", "NP")
person_vars <- c("RT", "SERIALNO", "PUMA", "ST", "SEX", "AGEP",  "SCH", "SCHG", "RELP", 
                 "HISP", "ESR", "PINCP", "NATIVITY", "OCCP", "POBP", "RAC1P")

pums_households_path <- read.csv("inst/extdata/10/input/pums/natstat/2013/puma/ss13hde.csv") 
pums_hh_subset <- pums_households_path[, household_vars]
write.csv(x = pums_hh_subset, file = "inst/extdata/10/input/pums/natstat/2013/puma/ss13hde.csv")

pums_persons <- read.csv("inst/extdata/10/input/pums/natstat/2013/puma/ss13pde.csv") 
pums_p_subset <- pums_persons[, person_vars]
write.csv(x = pums_p_subset, file = "inst/extdata/10/input/pums/natstat/2013/puma/ss13pde.csv")

# Poptable ---
poptab <- read.csv("inst/extdata/10/input/counts/natstat/2010/tract/households.csv")
poptab$county <- substr(poptab$Id2, start = 3, stop = 5)
poptab <- poptab[which(poptab$county == "001"), ]
write.csv(x = poptab, file = "inst/extdata/10/input/counts/natstat/2010/tract/households.csv")

# Roads ---

# Shapefiles ---
delaware_shape <- read_shapespatial_to_ogr("inst/extdata/10/input/shapefiles/natstat/2010/tract/tl_2010_10_tract10.shp")
delaware_shape_sub <- delaware_shape[which(delaware_shape$COUNTYFP10 == "001"), ]
writeOGR(obj = delaware_shape_sub, dsn = "inst/extdata/10/input/shapefiles/natstat/2010/tract", 
         driver="ESRI Shapefile", layer = "tl_2010_10_tract10")

# Marginals --- 

# Schools ---

# Workplaces ---
delaware_workplaces <- read.csv("inst/extdata/10/input/workplaces/natstat/2009/county/us.workplaces_2009_10.csv")
delaware_workplaces_sub <- delaware_workplaces[grep(pattern = "10001", x = delaware_workplaces$stcotr), ]
write.csv(delaware_workplaces_sub, file = "inst/extdata/10/input/workplaces/natstat/2009/county/us.workplaces_2009_10.csv")
  
# Uruguay ------------------------------------------------

# Pums ---
uruguay_pums <- read.csv(file = "inst/extdata/ury/input/pums/ipums/2011/1/uruguay.csv")

# 

