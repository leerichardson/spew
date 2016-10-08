import os
import shutil
import datetime 

# Set the base directory where all of the 
# outputs are located as well as the 
base_dir = "/mnt/beegfs1/data/shared_group_data/syneco/spew_1.2.0"
output_dir = "/mnt/beegfs1/data/shared_group_data/syneco/olympus/logfiles/logfiles_1.2.0"

# If the output directory exists, remove it and replace it 
# with a blank directory. If not, create it 
if os.path.isdir(output_dir):
	shutil.rmtree(output_dir)
os.makedirs(output_dir)

# Loop through the directory and check to see if it's 
# a logfile dir. If it is, then retrieve the most recent 
# logfile. If not, move to the next directory 
for dir_name, subdir_list, file_list in os.walk(base_dir):
	base_name = os.path.basename(dir_name)
	if base_name == "logfiles":
		# Set up a naive date in which every logfile will 
		# be larger than. That way, the first logfile wil automatically 
		# become the most recent 
		compare_date = datetime.date(1970, 1, 1)
		for file_name in file_list:
			full_path = dir_name + "/" + file_name
			date_string = file_name[8:14]
			date_obj = datetime.datetime.strptime(date_string, '%m%d%y').date()
			
			# Determine whether this log file is the most recent 
			if date_obj > compare_date:
				compare_date = date_obj
				most_recent_file = full_path
			print date_string
			print date_obj
			print "Current: " + file_name
			print "Most Recent: " + most_recent_file

		# Move the most recent logfiles to the output director
		print "Copying: " + most_recent_file + " to Outputs"
		shutil.copy(most_recent_file, output_dir)
