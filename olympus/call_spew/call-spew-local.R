devtools::load_all("/home/lee/Dropbox/spew")

input_dir <- "/home/lee/Dropbox/spew/inst/extdata/10/input"
output_dir <- "/home/lee/Dropbox/spew_output"
folders <- list(pop_table = "counts/natstat/2010/tract", 
	           pums = "pums/natstat/2013/puma", 
	           shapefiles = "shapefiles/natstat/2010/tract", 
	           roads = "roads/natstat/2010/county", 
	           schools = "schools/natstat/2013/county", 
	           lookup = "lookup/natstat/2010/tract", 
	           workplaces = "workplaces/natstat/2009/county", 
	           marginals = "marginals/natstat/2014/tract")
vars <- list(household = c("SERIALNO", "PUMA", "NP", "HINCP"), 
	         person = c("SERIALNO", "PUMA", "SEX", "AGEP", "RAC1P", "SCH", "SCHG", "ESR"))

# Run SPEW Locally ---
call_spew(input_dir = input_dir, 
		  output_dir = output_dir, 
		  folders = folders, 
		  vars = vars, 
		  run_type = "SEQ")
