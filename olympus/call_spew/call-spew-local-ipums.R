devtools::load_all("/home/lee/Dropbox/spew")


input_dir  <- "/home/lee/Dropbox/spew/inst/extdata/ury/input"
output_dir <- "/home/lee/Dropbox/spew/inst/extdata/ury/output"

folders <- list(pop_table = "counts", 
              pums = "pums", 
              shapefiles = "shapefiles")

vars = list(household = c("SERIAL", "COUNTRY","YEAR",,"PERSONS","GEOLEV1", "HHTYPE","PERNUM"), 
			person = c("SERIAL","AGE","SEX","RACE","SCHOOL","INCTOT"))

data_group = "ipums"
sampling_method <- "uniform"
locations_method <- "uniform"
convert_count <- TRUE
run_type = "MC"

call_spew(input_dir = input_dir, 
		  output_dir = output_dir, 
		  folders = folders, 
		  vars = vars, 
		  data_group = data_group,
		  sampling_method = sampling_method,
		  locations_method = locations_method,
		  run_type = run_type)

