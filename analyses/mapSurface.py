import glob, os, ast
import pygame

def init_datafile_mapSurface(expStartTime, subjectID, trialNum, filename=None):
	if filename is None:
		filename = os.path.join("ERC_WP3_Year1_Study1_") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_mapSurfaceNEW_trial_" + str(trialNum) + ".txt"
	  
def write_mapSurface(output, expStartTime, subjectID, trialNum, filename=None):
	if filename is None:
		filename = os.path.join("ERC_WP3_Year1_Study1_") + str(subjectID) + "_" + str(expStartTime) + "_visual_foraging_mapSurfaceNEW_trial_" + str(trialNum) + ".txt"
	f = open(filename, 'a')
	f.write("%s" % output)
	f.close()

# Read datafiles
all_datafiles = glob.glob('./*.txt')
select_datafiles = [file for file in all_datafiles if "_visual_foraging_mapSurface_trial_" in file]

# Melt dataframes to long format
for eachfile in select_datafiles:
	subjectID = os.path.splitext(os.path.basename(eachfile))[0][21:25]
	expStartTime = os.path.splitext(os.path.basename(eachfile))[0][26:40]
	trialNum = os.path.splitext(os.path.basename(eachfile))[0][-1:]
	init_datafile_mapSurface(expStartTime, subjectID, trialNum)
	array = []
	
	with open(eachfile, 'r') as file:
		mapSurface = pygame.Surface((200, 200), flags=0)
		mapSurface.fill((0, 0, 0))
		
		for line in file:
			coordall = ast.literal_eval(line)
			for coordlist in coordall:
				pygame.draw.polygon(mapSurface, (0, 0, 255), coordlist)
		
		size = width, height = 600, 600
		_display_surf = pygame.display.set_mode(size,
                                                     pygame.HWSURFACE |
                                                     pygame.DOUBLEBUF)# |
                                                     #pygame.FULLSCREEN)
		
		_display_surf.blit(pygame.transform.scale(mapSurface,(600, 600)), (0, 0)) # Uncomment to display mapSurface
		pygame.display.flip()
		pygame.time.delay(5000)

		for i in range(len(pygame.PixelArray(mapSurface))):
			#print str(list(pygame.PixelArray(mapSurface)[i]))[1:len(pygame.PixelArray(mapSurface)[i])] + "\n" 
			print len(pygame.PixelArray(mapSurface)[i])
			#mappedData = str(list(pygame.PixelArray(mapSurface)[i]))[1:len(pygame.PixelArray(mapSurface))] + "\n" 
			#write_mapSurface(mappedData, expStartTime, subjectID, trialNum)
