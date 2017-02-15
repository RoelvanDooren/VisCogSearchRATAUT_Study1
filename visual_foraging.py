#! /usr/bin/env python

import pygame
import random
import sys
from math import sin, cos, radians
import numpy as np
from pygame.locals import *
from timeit import default_timer as timer
import os

def rotatePolygon(polygon, theta):
    """Rotates the given polygon which consists of corners represented as (x,y),
    around the ORIGIN, clock-wise, theta degrees"""
    theta = radians(theta)
    rotatedPolygon = []
    for corner in polygon:
        rotatedPolygon.append((corner[0]*cos(theta)-corner[1]*sin(theta),
                               corner[0]*sin(theta)+corner[1]*cos(theta)))
    return rotatedPolygon


def movePolygon(polygon, x, y):
    """Moves the given polygon which consists of corners represented as (x, y)"""
    movedPolygon = []
    for corner in polygon:
        movedPolygon.append((corner[0]+x, corner[1]+y))
    return movedPolygon


def clockHand(size, theta, xloc, yloc):
    dx = int(cos(radians(-theta)) * size)
    dy = int(sin(radians(-theta)) * size)
    return xloc + dx, yloc + dy


class Agent:
    def __init__(self):
        self.position = (100.0, 100.0)
        self.speed = 0
        self.direction = random.randint(0, 359)
        self.total_turned = 0
        self.total_food = 0
        self.total_turned_after_food = 0
        self.last_food_time = 0
        self.food_encounter = 'false'
        self.last_angle = self.direction
        self.last_angle_timestamp = timer()
        self.timestamp_300ms_interval = 'NA'
        self.total_avg_turned = 0

    def move(self):
        self.position = (self.position[0] + self.speed * cos(radians(self.direction)),
                         self.position[1] + self.speed * sin(radians(self.direction)))
        self.position = np.clip(self.position, 0, 199)
        if self.position[0] == 0.0 or self.position[0] == 199.0:
			self.position[0] = (199.0 if self.position[0] == 0.0 else 0.0)
        if self.position[1] == 0.0 or self.position[1] == 199.0:
			self.position[1] = (199.0 if self.position[1] == 0.0 else 0.0)

class Wait:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 3
        self.font = pygame.font.Font(None, 25)

    def intro(self, image):
        self.surface.fill((0,0,0))
        self.surface.blit(image, (30, 100))
			
        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    return

    def totalScore(self, text, textpos, trial):
        self.surface.fill((0,0,0))
        self.surface.blit(text, textpos)
        if trial < 4:
			text = self.font.render("Druk op de spatiebalk om de volgende visuele zoektaak te starten.", 1, (200, 200, 200))
			textpos = text.get_rect(center=(self.surface.get_width() / 2,
                                            self.surface.get_height() / 2))
			self.surface.blit(text, textpos)
        else:
			text1 = self.font.render("Dit was de laatste ronde van de visuele zoektaak!", 1, (200, 200, 200))
			text2 = self.font.render("Druk op de spatiebalk om door te gaan naar de volgende taak.", 1, (200, 200, 200))
			textpos1 = text1.get_rect(center=(self.surface.get_width() / 2,
                                            self.surface.get_height() / 2))
			textpos2 = text2.get_rect(center=(self.surface.get_width() / 2,
                                            self.surface.get_height() / 1.2))
			
			self.surface.blit(text1, textpos1)
			self.surface.blit(text2, textpos2)
			
        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    return

class Environment:
    def __init__(self, expStartTime, subject, condition, trialnum):
        self.map = np.zeros((200, 200))
        self.expStartTime = expStartTime
        self.subjectID = subject
        self.condition = condition
        self.trialNum = trialnum

    def write_mapSurface(self, mapSurface, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_mapSurface_trial_" + str(self.trialNum+1) + ".txt"
        f = open(filename, 'a')
        f.write("%s" % mapSurface)
        f.close()

    def init_datafile_mapSurface(self, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_mapSurface_trial_" + str(self.trialNum+1) + ".txt"
          
    def drawnewcoords(self, coordall):
		overlaps = True
		while overlaps:
			xpos = random.randint(0+2, 200-2)
			ypos = random.randint(0+2, 200-2)
			coordlist = ((xpos-1, ypos), (xpos, ypos-1), (xpos, ypos), (xpos+1, ypos), (xpos, ypos+1))
			mapcoordlist = [x for eachlist in coordall for x in eachlist]
			if [x for x in coordlist if x in mapcoordlist] == []:
				overlaps = False
		return coordlist

    def gen_diffuse(self, num_patches=608): # 608 rather than 624 patches of 5 px each.
        mapSurface = pygame.Surface((200, 200), flags=0)
        self.init_datafile_mapSurface()
        overlaps = True        
        while overlaps:
			mapSurface.fill((0, 0, 0))
			coordall = []
			for i in range(num_patches):
				xpos = random.randint(0+2, 200-2)
				ypos = random.randint(0+2, 200-2)       
				coordlist = ((xpos-1, ypos), (xpos, ypos-1), (xpos, ypos), (xpos+1, ypos), (xpos, ypos+1))
				mapcoordlist = [x for eachlist in coordall for x in eachlist]
				while any([x for x in coordlist if x in mapcoordlist]):
					coordlist = self.drawnewcoords(coordall)
				coordall.append(coordlist)
				pygame.draw.polygon(mapSurface, (0, 0, 255), coordlist)
			
			pxarray = pygame.PixelArray(mapSurface)
			if np.count_nonzero(np.array(pxarray) == 0) == 36960:  # If patches don't overlap there are 36960 black pixels / 3040 green pixels
				overlaps = False
        self.write_mapSurface(coordall)
        return mapSurface

    def gen_patchy(self, num_patches=4):
        mapSurface = pygame.Surface((200, 200), flags=0)
        self.init_datafile_mapSurface()
        overlaps = True
        while overlaps is True:
            mapSurface.fill((0, 0, 0))
            coordall = []
            
            for i in range(num_patches):
                xpos = random.randint(0+20, 200-20)
                ypos = random.randint(0+20, 200-20)
                coordlist = ((xpos-19, ypos), (xpos, ypos-19), (xpos+19, ypos), (xpos, ypos+19))

                coordall.append(coordlist)
                pygame.draw.polygon(mapSurface, (0, 0, 255), coordlist)
                
            pxarray = pygame.PixelArray(mapSurface)
            if np.count_nonzero(np.array(pxarray) == 0) ==36956:  # If patches don't overlap there are 36956 black pixels / 3044 green pixels
				overlaps = False
        self.write_mapSurface(coordall)
        return mapSurface

class App:
    def __init__(self, expStartTime, subject, condition, trial_time, debug=False):
        self._running = True
        self._display_surf = None
        self.debug = debug
        self.size = self.width, self.height = 600, 600
        self.expStartTime = expStartTime
        self.subjectID = subject
        self.condition = condition
        self.total_counter = 2400
        self.update_counter = 2400
        self.food_encounter_previous_xycoord = ''
        self.trialNum = 0
        self.trialStartTime = 0
        self.trial_time = trial_time
        self.path_array = []
        
        if self.debug is True:
            self.visiblePath = True
        else:
            self.visiblePath = False

    def draw_info_overlay(self):
        loc = (int(round(self.agent.position[0]*3)),
               int(round(self.agent.position[1]*3)))
               
        polygon_points = rotatePolygon([[0, 10], [-5, -10], [5, -10]],(self.agent.direction + 270) % 360)
        polygon_points = movePolygon(polygon_points, loc[0], loc[1])
        pygame.draw.polygon(self._display_surf, (255, 255, 255), polygon_points, 0)

        if timer() - self.trialStartTime < 3:
            font = pygame.font.Font(None, 40)
            text = font.render("Klaar?", 1, (200, 200, 200))
            textpos = text.get_rect(center=(self._display_surf.get_width() / 2,
                                            self._display_surf.get_height() / 3))
            self._display_surf.blit(text, textpos)

        if timer() - self.trialStartTime >= 3:
			font = pygame.font.Font(None, 40)
			text = font.render("Score: " + str(self.agent.total_food), 1, (100, 100, 100))
			counter = font.render(str(self.update_counter), 1, (100, 100, 100))
			textpos = text.get_rect(topleft=(10, 10))
			counterpos = counter.get_rect(topright=(535, 10))
			self._display_surf.blit(text, textpos)
			self._display_surf.blit(counter, counterpos)
			pygame.draw.circle(self._display_surf, (128, 128, 128),(self._display_surf.get_width() - 30, 30), 15)
			xpos, ypos = clockHand(15, (timer()*180) % 360, self._display_surf.get_width() - 30, 30)
			pygame.draw.line(self._display_surf, (255, 255, 255),(self._display_surf.get_width() - 30, 30),(xpos, ypos), 2)
		
    def zerofill(self, number, width):	
		width -= len(str(number))
		if (width > 0) :
			i = 0
			while i < width:
				number =   str(number).join("0")+ str(number)
				i += 1
		return number
			
    def init_datafile(self, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging"+".txt"
        f = open(filename, 'w')
        output = 'expStartTime,subjectID,condition,trial_num,timespent,food_eaten,turn_angle,angle_after_food,total_avg_turned\n'
        f.write(output)
        f.close()
        
    def init_datafile_path(self, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_trial_" + str(self.trialNum+1) + ".txt"
            
    def write_data(self, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging"+".txt"
        f = open(filename, 'a')
        output = str(self.expStartTime) + "," + \
				 str(self.subjectID) + "," + \
                 self.condition + "," + \
                 str(self.trialNum+1) + "," + \
                 str(timer() - self.trialStartTime - 3) + "," + \
                 str(self.agent.total_food) + "," + \
                 str(self.agent.total_turned) + "," + \
                 str(self.agent.total_turned_after_food) + "," + \
                 str(self.agent.total_avg_turned) + "\n"
        f.write(output)
        f.close()
        
    def write_data_path(self, output, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_trial_" + str(self.trialNum+1) + ".txt"
        f = open(filename, 'a')
        f.write("%s" % output)
        f.close()
        
    def on_init(self):
        pygame.init()
        self.subjectID = self.zerofill(self.subjectID, 4)
        self.init_datafile()
        self.init_datafile_path()
        self.clock = pygame.time.Clock()
        self._display_surf = pygame.display.set_mode(self.size,
                                                     pygame.HWSURFACE |
                                                     pygame.DOUBLEBUF |
                                                     pygame.FULLSCREEN)
        self.wait = Wait(self._display_surf)

    def on_event(self, event):
        if event.type == pygame.QUIT:
            self._running = False
        elif event.type == KEYDOWN and event.key == K_j:
            self.agent.direction = (self.agent.direction - 35)%360
            self.agent.total_turned += 35
            if timer() - self.agent.last_food_time < 0.3 and self.agent.last_food_time > 0:
                self.agent.total_turned_after_food += 35
        elif event.type == KEYDOWN and event.key == K_l:
            self.agent.direction = (self.agent.direction + 35)%360
            self.agent.total_turned += 35
            if timer() - self.agent.last_food_time < 0.3 and self.agent.last_food_time > 0:
                self.agent.total_turned_after_food += 35

    def on_loop(self):
        pygame.mouse.set_visible(False)
        if self.agent.speed == 0 and timer() - self.trialStartTime >= 3:
            self.agent.speed = 0.333
        self.agent.move()
        
        if timer() - self.trialStartTime >= 3:
			self.update_counter = int((self.total_counter/(self.trial_time-3.0)) * (self.trial_time - (timer()- self.trialStartTime)))
			
			if timer() - self.agent.last_angle_timestamp > 0.3:
				self.agent.total_avg_turned += abs((((self.agent.direction - self.agent.last_angle) + 180) % 360) - 180)
				self.agent.last_angle = self.agent.direction
				self.agent.last_angle_timestamp = timer()
				self.agent.timestamp_300ms_interval = 'START'
			else: 
				self.agent.timestamp_300ms_interval = 'INTERVAL'
        position = (int(round(self.agent.position[0])),
                    int(round(self.agent.position[1])))
        mappxarray = pygame.PixelArray(self.mapSurface)
        seenpxarray = pygame.PixelArray(self.seenSurface)
        if self.visiblePath is True: seenpxarray[position[0], position[1]] = pygame.Color(255, 255, 255)
        if mappxarray[position[0], position[1]] > 0:
            if seenpxarray[position[0],position[1]] == 0:
				self.agent.total_food += 1
				self.agent.last_food_time = timer()
				self.agent.food_encounter = 'true'
				self.food_encounter_previous_xycoord = 'false'
            else:
				self.agent.food_encounter = 'false'
				self.food_encounter_previous_xycoord = 'true'
            seenpxarray[position[0], position[1]] = pygame.Color(0, 255, 0)
            
        else:
			self.agent.food_encounter = 'false'
			self.food_encounter_previous_xycoord = 'false'
        
    def on_render(self):	
		black = 0, 0, 0
		self._display_surf.fill(black)
		self._display_surf.blit(pygame.transform.scale(self.seenSurface,(600, 600)), (0, 0))
		#self._display_surf.blit(pygame.transform.scale(self.mapSurface,(600, 600)), (0, 0)) # Uncomment to display mapSurface
		self.draw_info_overlay()
		pygame.display.flip()

    def on_cleanup(self):
        pygame.quit()

    def run_trial(self, trialNum):
        self._running = True
        self.trialNum = trialNum
        self.env = Environment(self.expStartTime, self.subjectID, self.condition, self.trialNum)
        if self.condition == "d":
            self.mapSurface = self.env.gen_diffuse()
        elif self.condition == "c":
            self.mapSurface = self.env.gen_patchy()
        self.seenSurface = pygame.Surface((200, 200), flags=0)
        self.seenSurface.fill((0, 0, 0))
        self.agent = Agent()
        self.trialStartTime = timer()
        while self._running:
            self.clock.tick(60)
            for event in pygame.event.get():
                self.on_event(event)
            self.on_loop()
            self.on_render()
            self.path_array.append([self.expStartTime, self.subjectID, self.condition, self.trialNum+1, timer() - self.trialStartTime - 3, 
								  self.agent.position[0], self.agent.position[1], self.agent.direction, self.agent.last_angle, self.agent.timestamp_300ms_interval, self.agent.total_avg_turned,
								  self.agent.food_encounter, self.food_encounter_previous_xycoord, self.agent.total_food, self.update_counter])
				
            if timer()-self.trialStartTime >= self.trial_time:
				self._running = False
		

        font = pygame.font.Font(None, 40)
        textScore = font.render("Je totale score deze ronde is: " + str(self.agent.total_food), 1, (200, 200, 200))
        textpos = textScore.get_rect(center=(self._display_surf.get_width() / 2,
                                            self._display_surf.get_height() / 2.5))
        self.wait.totalScore(textScore, textpos, trialNum)

        self.write_data()
        self.write_data_path(self.path_array)		
        self.path_array = []
		

    def on_execute(self):
        if self.on_init() == False:
            self._running = False
            
        image_intro01 = pygame.image.load(os.path.join("images", "intro_foraging.png")).convert()
        self.wait.intro(image_intro01)

        for trialNum in range(5):
			self.update_counter = 2400
			self.run_trial(trialNum)


        self.on_cleanup()

if __name__ == "__main__":
    debug = sys.argv[4]
    if debug == "f":
        trial_time = 123.0
    else:
        trial_time = 5.0
	
    expStartTime = sys.argv[1]
    subject_ID = sys.argv[2]
    if sys.argv[3] == "r":
        if int(subject_ID) % 2 == 1:
			condition = "c"
        else:
			condition = "d"
    else:
        condition = sys.argv[3]

    theApp = App(expStartTime, subject_ID, condition, trial_time, debug=False)
    theApp.on_execute()
