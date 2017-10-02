	# Lee Richardson 
# Script to Generate SPEW_1.2.0 Ecosystems on Olympus
# November 4, 2016
import os 
import csv
import time 
import stat

homedir = os.environ['HOME']
print homedir
if homedir != "/home/lee":
	homedir = "/mnt/beegfs1/data/shared_group_data/syneco"
else:
	homedir = "/home/lee/Dropbox"

print "Home Directory: " + homedir

# Shell script to call SPEW
call_spew = homedir + "/spew/olympus/call_spew/call_spew.sh"
print "Call SPEW Script: " + call_spew

# Filepath for the SPEW hierarchy 
spew_hierarchy =  homedir + "/spew/olympus/spew_hierarchy/spew_hierarchy.csv"

# Set the input directory 
input_base = homedir + "/spew_input"

# Filepath for all United States Directories 
us_hierarchy = homedir + "/spew/olympus/spew_hierarchy/country_hierarchies/usa_lookup.csv"
print us_hierarchy
us_base_dir = "americas/northern_america/usa"

# Call the rest of the United States 
run_type = "MPI"
with open(us_hierarchy, 'rb') as us_csv:	
	us_hier = csv.reader(us_csv) 
	for us_row in us_hier:
		state = us_row[0]
		if state == "geoid":
			continue
		
		state_path = us_base_dir + "/" + state
		state_call = "bash " + call_spew + " " + state_path + " US " + run_type 
		print state_call
		if homedir != "/home/lee/Dropbox":
			os.system(state_call)


# Call all IPUMS country, as well as a special condition for Canada 
run_type = "MC"
with open(spew_hierarchy, 'rb') as spew_csv:
	spew_hier = csv.reader(spew_csv)
	for row in spew_hier:
		# Construct file path for the country 	
		country = row[0]
		if country == "country_name": # Skip the first (title) row of the .csv
			continue
		iso3 = row[2]
		if iso3 == "lca" or iso3 == "gbr" or iso3 == "usa": 
			continue

		region = row[4]
		sub_region = row[5]
		country_path = region + "/" + sub_region + "/" + iso3

		# Skip if there is no PUMS input data
		print homedir
		if homedir != "/home/lee/Dropbox":
			input_dir = input_base + "/" + country_path + "/input"
			print "Input Directory for " + country + ": " + input_dir
			inputs = os.listdir(input_dir)
			if "pums" not in inputs:
				continue

		ipums_call = "bash " + call_spew + " " + country_path + " ipums " + run_type 
		print ipums_call
		
		if homedir != "/home/lee/Dropbox":
			os.system(ipums_call)
