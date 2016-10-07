import os
import csv

# Path to the shell script which calls spew on Olympus 
call_spew = "/mnt/beegfs1/data/shared_group_data/syneco/olympus/call_spew/call_spew.sh"

# Set the base SPEW directory 
spew_dir = "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0"

# Set the SPEW_hierarchy file-path
hierarchy =  "/mnt/beegfs1/data/shared_group_data/syneco/olympus/spew_hierarchy/spew_hierarchy.csv"

# Loop through all the countries in the hierarchy
total = 0
with open(hierarchy, 'rb') as spew_csv:
	spew_hier = csv.reader(spew_csv)
	for row in spew_hier:
		# Skip the first row of the hierarchy  
		country = row[0]
		if country == "country_name":
			continue

		# Consruct the file-path to the country directory 
		iso3 = row[2]
		if iso3 == "iso3":
			continue
		region = row[4]
		sub_region = row[5]
		country_dir = spew_dir + "/" + region + "/" + sub_region + "/" + iso3
		
		# Check for input data 
		dirs = os.listdir(country_dir)
		if "input" not in dirs:
			print country + " has no input data!"
			continue

		# Check for IPUMS data 
		input_dir = country_dir + "/input"
		inputs = os.listdir(input_dir)
		if "pums" not in inputs:
			continue

		# Check if it's saint_lucia or the UK
		if iso3 == "lca" or iso3 == "gbr" or iso3 == "usa" or iso3 == "can":
			continue

		total = total + 1
		print "Running: " + country + " number: " + str(total)

		# Call SPEW 
		call_dir = "spew_1.2.0/" + region + "/" + sub_region + "/" + iso3
		data_group = "ipums"
		shell_call = "bash " + call_spew + " " + call_dir + " " + data_group
		os.system(shell_call)