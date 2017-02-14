#! /usr/bin/env python
import glob, os, ast

def init_datafile_path(expStartTime, subjectID, trialNum, eachfile, filename=None):
	if filename is None:
		filename = os.path.join("ERC_WP3_Year1_Study1_") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_path_array_" + ("practice_" if "practice" in eachfile else "") + "trial" + ("_" if not "practice" in eachfile else "") + str(trialNum) + ".txt"
	f = open(filename, 'w')
	output = ('expStartTime, subjectID, condition, trial_num, timespent, xcoord, ycoord, collision_encountered, total_collisions\n' if "practice_" in eachfile else 'expStartTime, subjectID, condition, trial_num, timespent, xcoord, ycoord, current_angle, resource_encountered, resource_encountered_here_before, food_eaten, timesteps_left\n')
	f.write(output)
	f.close()

def write_data_path(output, expStartTime, subjectID, trialNum, eachfile, filename=None):
	if filename is None:
		filename = os.path.join("ERC_WP3_Year1_Study1_") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_path_array_" + ("practice_" if "practice" in eachfile else "") + "trial" + ("_" if not "practice" in eachfile else "") + str(trialNum) + ".txt"
	f = open(filename, 'a')
	f.write("%s\n" % ''.join([l for l in str(output) if l not in ("'"," ")]))
	f.close()

# Read datafiles
all_datafiles = glob.glob('./*.txt')
select_datafiles = [file for file in all_datafiles if "_visual_foraging_path_trial_" in file] + [file for file in all_datafiles if "visual_foraging_path_practice_trial" in file]

# Melt dataframes to long format
for eachfile in select_datafiles:
	print eachfile
	subjectID = os.path.splitext(os.path.basename(eachfile))[0][21:25]
	expStartTime = os.path.splitext(os.path.basename(eachfile))[0][26:40]
	trialNum = ("" if "practice_" in eachfile else os.path.splitext(os.path.basename(eachfile))[0][-1:])
	init_datafile_path(expStartTime, subjectID, trialNum, eachfile)
	
	with open(eachfile, 'r') as file:
		for line in file:		
			allrows = ast.literal_eval(line)
			for eachrow in allrows:
				write_data_path(str(eachrow).strip('[]'), expStartTime, subjectID, trialNum, eachfile)
