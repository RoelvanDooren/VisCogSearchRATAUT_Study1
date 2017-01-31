#! /usr/bin/env python

import pygame, random, sys, os
from math import sin, cos, radians
import numpy as np
from pygame.locals import *
from timeit import default_timer as timer
import time
import os



def rotatePolygon(polygon,theta):
    """Rotates the given polygon which consists of corners represented as (x,y),
    around the ORIGIN, clock-wise, theta degrees"""
    theta = radians(theta)
    rotatedPolygon = []
    for corner in polygon:
        rotatedPolygon.append(( corner[0]*cos(theta)-corner[1]*sin(theta) , corner[0]*sin(theta)+corner[1]*cos(theta)) )
    return rotatedPolygon

def movePolygon(polygon,x,y):
    """Moves the given polygon which consists of corners represented as (x,y)"""
    movedPolygon = []
    for corner in polygon:
        movedPolygon.append(( corner[0]+x , corner[1]+y))
    return movedPolygon

class Agent:
    def __init__(self):
        self.position = (7.5, 50.0)
        self.speed = 0
        self.direction = 0
        self.total_counter = 2400
        self.total_turned = 0
        self.total_collisions = 0
        self.last_angle = self.direction
        self.last_angle_timestamp = timer()
        self.total_avg_turned = 0
        self.collision_encounter = "False"
	
    def move(self):
        self.position = (self.position[0] + self.speed * cos(radians(self.direction)),
                         self.position[1] + self.speed * sin(radians(self.direction)))
                 
        self.position = np.clip(self.position, 0, 199)

class Wait:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 3
        self.font = pygame.font.Font(None, 25)

    def intro(self, image):
        self.surface.fill((255, 255, 255))
        self.surface.blit(image, (30, 100))
			
        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    return

class Environment:
    def __init__(self):
        self.map = np.zeros((200, 200))
	
    def gen_practice(self):
		maze = pygame.image.load(os.path.join("images", "maze_foraging_practice.bmp")).convert()
		mapSurface = pygame.transform.scale(maze, (200, 200))
		return mapSurface

class App:
    def __init__(self, expStartTime, subject, condition, ghost, debug=False):
        self._running = True
        self._display_surf = None
        self.debug = debug
        self.size = self.width, self.height = 600, 600
        self.ghost = ghost
        self.expStartTime = expStartTime
        self.subjectID = subject
        self.condition = condition
        self.trialStartTime = 0
        self.trialNum = "practice"
        self.path_array = []
        if self.debug is True:
            self.visiblePath = True
        else:
            self.visiblePath = False

    def draw_info_overlay(self):
        loc = (int(round(self.agent.position[0]*3)), int(round(self.agent.position[1]*3)))
        polygon_points = rotatePolygon([[0, 10], [-5, -10], [5, -10]], (self.agent.direction + 270)%360)
        polygon_points = movePolygon(polygon_points,loc[0],loc[1])
        pygame.draw.polygon(self._display_surf, (255, 255, 255), polygon_points, 0)
	
        if timer() - self.trialStartTime < 3:
			font = pygame.font.Font(None, 40)
			text = font.render("Klaar?", 1, (200, 200, 200))
			textpos = text.get_rect(center=(self._display_surf.get_width() / 2, self._display_surf.get_height() / 2.5))
			self._display_surf.blit(text, textpos)
		
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
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_practice"+".txt"
        f = open(filename, 'w')
        output = 'expStartTime,subjectID,trial_num,timestamp,total_collisions,turn_angle,total_avg_turned\n'
        f.write(output)
        f.close()

    def init_datafile_path(self, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_practice_trial.txt"

    def write_data(self, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_practice"+".txt"
        f = open(filename, 'a')
        output = str(self.expStartTime) + "," + str(self.subjectID) + "," + str(self.trialNum) + "," + \
                str(timer() - self.trialStartTime-3.0) + "," + str(self.agent.total_collisions) + \
                "," + str(self.agent.total_turned) + "," + \
                 str(self.agent.total_avg_turned) + "\n"
        f.write(output)
        f.close()

    def write_data_path(self, output, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_visual_foraging_path_practice_trial.txt"
        f = open(filename, 'a')
        f.write("%s" % output)
        f.close()

    def on_init(self):
        pygame.init()
        self.subjectID = self.zerofill(self.subjectID, 4)
        self.init_datafile()
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
        elif event.type == KEYDOWN and event.key == K_l:
            self.agent.direction = (self.agent.direction + 35)%360
            self.agent.total_turned += 35

    def on_loop(self):
        pygame.mouse.set_visible(False)
        if self.agent.speed == 0 and timer() - self.trialStartTime > 3:
            self.agent.speed = 0.333
        self.agent.move()
               
        if timer() - self.agent.last_angle_timestamp > 0.3:
            self.agent.total_avg_turned += abs((((self.agent.direction - self.agent.last_angle) + 180) % 360) - 180)
            self.agent.last_angle = self.agent.direction
            self.agent.last_angle_timestamp = timer()
        position = (int(round(self.agent.position[0])),int(round(self.agent.position[1])))
        mappxarray = pygame.PixelArray(self.mapSurface)
        seenpxarray = pygame.PixelArray(self.seenSurface)

        if self.visiblePath is True: seenpxarray[position[0], position[1]] = pygame.Color(255, 255, 255)
        if mappxarray[position[0],position[1]] !=0 and mappxarray[position[0],position[1]] != 2273612: # Detects the white borders (== 16777215) of the maze
            if seenpxarray[position[0],position[1]] == 0:
				self.agent.total_collisions += 1
				seenpxarray = pygame.PixelArray(self.seenSurface) # Reset pixel array
				if self.ghost == "false":
					self.agent.position = (7.5, 50.0) # Reset position of polygon
					self.agent.collision_encounter = "True"
        else:
			self.agent.collision_encounter = "False"	
        if mappxarray[position[0],position[1]] == 2273612: # Detects the maze exit
			self._running = False
                
    def on_render(self):
		black = 0, 0, 0
		self._display_surf.fill(black)
		self._display_surf.blit(pygame.transform.scale(self.seenSurface, (600, 600)), (0,0))
		self._display_surf.blit(pygame.transform.scale(self.mapSurface, (600, 600)), (0,0))
		self.draw_info_overlay()
		pygame.display.flip()

    def on_cleanup(self):
        pygame.quit()

    def run_practice(self):
		self._running = True
		self.env = Environment()
		self.mapSurface = self.env.gen_practice()
		self.seenSurface = pygame.Surface((200, 200), flags=0)
		self.seenSurface.fill((0, 0, 0))
		self.agent = Agent()
		self.trialStartTime = timer()
		while (self._running):
			self.clock.tick(60)
			for event in pygame.event.get():
				self.on_event(event)
			self.on_loop()
			self.on_render()
			
			if timer()-self.trialStartTime >= 3:
				self.path_array.append([self.expStartTime, self.subjectID, self.condition, self.trialNum, timer() - self.trialStartTime, 
								  self.agent.position[0], self.agent.position[1],
								  self.agent.collision_encounter, self.agent.total_collisions])
				
		self.write_data()
		self.write_data_path(self.path_array)		

    def on_execute(self):
        if self.on_init() == False:
            self._running = False
        
        self.subjectID = self.zerofill(self.subjectID, 4)
        image_intro01 = pygame.image.load(os.path.join("images", "intro_practice_foraging.png")).convert()
        self.wait.intro(image_intro01)
        self.run_practice()

        self.on_cleanup()

if __name__ == "__main__":
    debug = sys.argv[4]
    if debug == "f":
		ghost = "false"
    else:
        ghost = "true"
    
    expStartTime = sys.argv[1]
    subject_ID = sys.argv[2]
    if sys.argv[3] == "r":
        if int(subject_ID) % 2 == 1:
			condition = "c"
        else:
			condition = "d"
    else:
        condition = sys.argv[3]

    theApp = App(expStartTime, subject_ID, condition, ghost, debug=False)
    theApp.on_execute()

