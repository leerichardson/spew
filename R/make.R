# Pseudocode -----------------------

# Input: (POPTABLE: (PUMS sampling ID, Place ID, Number of households to sample), 
#         shapefile, PUMS (household, people))

#  For each place ID (tract)

# 1. Sample Synthetic Households 
###  Sampling from pums_h, which is passed from format(), by PUMA
###  Sampling scheme:  Currently sample uniformly over all households 
###    in the PUMS Sampling ID (PUMA) of that Place ID (tract)
###  Sampling size:  Household count for that place ID (tract)
###    which we get from households.csv (which goes into pop_table)
###  Note / TODO:  Want to update uniform sampling to do something better eventually

# 2. Sample the locations and assign to households
###  Sample scheme:  
###  -- Uniform over the geographic of the place ID (tract)
###  -- We use spsample() for this (package sp)
###  Sample one location for each household
###  Result:  (x,y) or (long,lat) for each household in the place ID (tract)

# 3. Assign synthetic people to the synthetic households
###  For each sampled synthetic household, we just take the people in the original household
###    using pums_p annd pums_h and the household ID number
###  Note / TODO:  We want to do something more elegant than this, such as
###    estimating some joint distirbution of person-level features in each region
###    and them sampling from that distribution

# 4. Format the output (two files)
###  Should look identical to pums_p and pums_h
###  Subset the columns depending on user input
###  Note / TODO:  For ipums, we may have to de-identify the records
###    so we may need to hash the old IDs to new IDs
###  Final Output: Written .csv file for each Place ID
