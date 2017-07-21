# Lee Richardson 
# Script to Generate SPEW_1.2.0 Ecosystems on Olympus
# November 4, 2016
import os 
import csv
import time 
import stat
# Shell script to call SPEW
call_spew = "/mnt/beegfs1/data/shared_group_data/syneco/olympus/call_spew/call_spew.sh"

# Filepath for the SPEW hierarchy 
spew_hierarchy =  "/mnt/beegfs1/data/shared_group_data/syneco/olympus/spew_hierarchy/spew_hierarchy.csv"

# Filepath for all United States Directories 
us_hierarchy = "/mnt/beegfs1/data/shared_group_data/syneco/olympus/spew_hierarchy/country_hierarchies/usa_lookup.csv"
us_base_dir = "spew_1.2.0/americas/northern_america/usa"

# Call California, New-York Specially with a specified number of NODES 
cali_call = "bash " + call_spew + " " + us_base_dir + "/06 US MPI 16"
texas_call = "bash " + call_spew + " " + us_base_dir + "/48 US MPI 6"
print cali_call
print texas_call
#os.system(cali_call)
time.sleep(10)
os.system(texas_call)
time.sleep(10)

# Call the rest of the United States 
with open(us_hierarchy, 'rb') as us_csv:	
	us_hier = csv.reader(us_csv) 
	for us_row in us_hier:
		nodes = "2"
		parallel_type = "MPI"
		time.sleep(3)
		
		# Skip if this is the first row of the .csv
		state = us_row[0]
		if state == "geoid":
			continue

		# Skip Large states because we have already called them
		if state == "06" or state == "48":
			print "Already called California and Texas!"
			continue

		# Add extra nodes for some of the larger states!
		if state == "42" or state == "12" or state == "36" or state == "17":
			nodes = "4"

		# Run the smaller states on one node!
		if state == "02" or state == "08" or state == "09" or state == "38" or state == "46" or state == "44" or state == "56" or state == "50" or state == "30":
			nodes = "1"

		state_path = us_base_dir + "/" + state
		state_call = "bash " + call_spew + " " + state_path + " US " + parallel_type + " " + nodes
		print state_call
		os.system(state_call)

# Call all IPUMS country, as well as a special condition for Canada 
with open(spew_hierarchy, 'rb') as spew_csv:
	spew_hier = csv.reader(spew_csv)
	for row in spew_hier:
		# Skip the first row of the .csv
		country = row[0]
		if country == "country_name":
			continue
		time.sleep(3)

		# Construct the filepath to the specific country 	
		iso3 = row[2]
		region = row[4]
		sub_region = row[5]
		country_path = "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/" + region + "/" + sub_region + "/" + iso3

		# Skip if there is no PUMS input data
		input_dir = country_path + "/input"
		inputs = os.listdir(input_dir)
		if "pums" not in inputs:
			continue

		# Skip Saint Lucia, Great Britain, and the USA 
		if iso3 == "lca" or iso3 == "gbr" or iso3 == "usa":
			continue
		
		# Set the input path for this particular SPEW call
		data_group = "ipums"
		call_dir = "spew_1.2.0/" + region + "/" + sub_region + "/" + iso3

		# Run the Canada synthetic ecosystem separately 
		if iso3 == "can":
			data_group = "none"
			canada_call = "bash " + call_spew + " " + call_dir + " " + data_group + " SOCK 1"
			os.system(canada_call)
			continue

		# Construct bash call for eligible ipums country 		
		ipums_call = "bash " + call_spew + " " + call_dir + " " + data_group + " MC 1"
		os.system(ipums_call)
