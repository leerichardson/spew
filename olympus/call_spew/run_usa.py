import os 

# Set the directory of the USA input data and the shell script to call spew
us_base_dir = "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0/americas/northern_america/usa"
base_dir_call = "spew_1.2.0/americas/northern_america/usa"
call_spew = "/mnt/beegfs1/data/shared_group_data/syneco/olympus/call_spew/call_spew.sh"

# Run call_spew.sh script, using the directory names 
us_dirs  = os.listdir(us_base_dir)
for us_dir in us_dirs:
	# Skip the non-state directories 
	if us_dir == "input" or us_dir == "output" or us_dir == "logfiles":
		continue

	# Construct the shell command, and call_spew for this US directory 
	base_dir = base_dir_call + "/" + us_dir
	data_group = "US"
	shell_call = "bash " + call_spew + " " + base_dir + " " + data_group + " MPI"
	os.system(shell_call)
