#! /usr/bin/env python

import glob, os, ast
import pygame

def init_datafile_mapSurface(expStartTime, subjectID, trialNum, filename=None):
	if filename is None:
		filename = os.path.join("../Processeddata/ERC_WP3_Year1_Study1_") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_mapSurface_recreated_trial_" + str(trialNum) + ".txt"
	  
def write_mapSurface(output, expStartTime, subjectID, trialNum, filename=None):
	if filename is None:
		filename = os.path.join("../Processeddata/ERC_WP3_Year1_Study1_") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_mapSurface_recreated_trial_" + str(trialNum) + ".txt"
	f = open(filename, 'a')
	f.write("%s" % output)
	f.close()

# Read datafiles
all_datafiles = glob.glob('./Temporary/*.txt')
select_datafiles = [file for file in all_datafiles if "_visual_foraging_mapSurface_trial_" in file]

# Melt dataframes to long format
for eachfile in select_datafiles:
	subjectID = os.path.splitext(os.path.basename(eachfile))[0][21:25]
	expStartTime = os.path.splitext(os.path.basename(eachfile))[0][26:40]
	trialNum = os.path.splitext(os.path.basename(eachfile))[0][-1:]
	init_datafile_mapSurface(expStartTime, subjectID, trialNum)
		
	with open(eachfile, 'r') as file:
		mapSurface = pygame.Surface((200, 200), flags=0)
		mapSurface.fill((0, 0, 0))
		for line in file:
			coordall = ast.literal_eval(line)
			# Reconstruct environment
			for coordlist in coordall:
				pygame.draw.polygon(mapSurface, (0, 0, 255), coordlist)
		for i in range(len(pygame.PixelArray(mapSurface))):
			mappedData = str(list(pygame.PixelArray(mapSurface)[i])[0:len(str(list(pygame.PixelArray(mapSurface)[i])))]).strip('[]') + "\n" 
			write_mapSurface(mappedData, expStartTime, subjectID, trialNum)
