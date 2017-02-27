#! /usr/bin/env python

import pygame
import string
import os
import sys
import random
from timeit import default_timer as timer

# Constants
color_font = (30, 30, 30)
background_color = (255, 255, 255)
box_edge_color = (0, 0, 0)
box_back_color = (240, 240, 240)
box_text_color = (100, 100, 100)
box_width = 600
box_height = 300
button_height = 35
screen_w = 640
screen_h = 500
screen = pygame.display.set_mode((screen_w, screen_h), pygame.HWSURFACE |
                                 pygame.DOUBLEBUF | pygame.FULLSCREEN)

# Variables
stimulus_set = []
correct_words = []


class Stimulus:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.font = pygame.font.Font(None, 60)

    def draw_word(self, word):
        self.surface.fill(background_color)
        text = self.font.render(word, 1, color_font)
        textpos = text.get_rect(center=(self.surface.get_width() / 2,
                                        self.surface.get_height() / 4))
        self.surface.blit(text, textpos)


class Input:
    """This class takes care of user input"""
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.current_string = []
        self.past_word = []
        self.timeend = 0
        self.previous_string = ""
        self.startTyping = 0
        self.font = pygame.font.SysFont("couriernew", 35)
        self.font_text = pygame.font.Font(None, 20)

    def draw_text_box(self, message):
        pygame.draw.rect(self.surface, box_back_color,
                         ((self.x - (box_width / 2)), self.y - 80,
                          box_width, box_height), 0)
        pygame.draw.rect(self.surface, box_edge_color,
                         ((self.x - (box_width / 2)), self.y - 80,
                          box_width, box_height), 1)
                                            
        if len(message) != 0:
            numlines = len(message) / 25.0
            ystart = 70
            
            self.surface.blit(self.font.render(message[0:25], 1, box_text_color),
										(self.x - 280, self.y - ystart))
           
            if numlines > 1.0:
				startvalue = 25
				endvalue = 50
				ystart = 45
				for eachint in range(int(numlines)):
					start = startvalue + (25*eachint)
					end = endvalue + (25*eachint)
					self.surface.blit(self.font.render(message[start:end], 1, box_text_color),(self.x - 280, self.y - ystart))					
					ystart = ystart - 25
		
        pygame.display.flip()

    def draw_input(self, eachword, trialStartTime, trial_time):
		event = pygame.event.poll()
		if event.type == pygame.KEYDOWN:
			if event.key == pygame.K_t:
				self.startTyping = timer()
				while True:
					event = pygame.event.poll() 		
					if timer()-trialStartTime >= (trial_time/2.0):
						return
					if event.type == pygame.KEYDOWN:
						if event.key == pygame.K_BACKSPACE:
							self.current_string = self.current_string[:-1]
						elif event.key == pygame.K_RETURN:
							self.previous_string = "".join(self.current_string)
							self.current_string = []
							self.timeend = timer()
				
							return self.previous_string
							
						elif event.key <= 127:
							self.current_string.append(chr(event.key))

					self.draw_text_box(string.join(self.current_string, ""))

class Wait:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.font = pygame.font.Font(None, 25)

    def intro(self, image):
        self.surface.fill(background_color)
        self.surface.blit(image, (50, 50))

        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    return

class Main:
    def __init__(self, words, expStartTime, subjectID, condition, taskorder, objectorder, trial_time):
		# Init data collection
        self._running = True
        self.start_time = timer()
        self.expStartTime = expStartTime
        self.subjectID = subjectID
        self.condition = condition
        self.participant_input = []
        self.trial_time = trial_time
        self.set_counter = -1
        self.numanswers = 0
        self.finishedword = "False"
        self.taskorder = taskorder
        self.objectorder = objectorder
        self.time = 0

        self.subjectID = self.zerofill(self.subjectID, 4)
        filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_alternate_uses_task"+".txt"
        f = open(filename, 'w')
        output = 'expStartTime;subjectID;condition;taskorder;objectorder;nth_word;word;trialStart;time_start;time_end;' \
                 'response_num;response;responsetime;finishedresponse\n'
        f.write(output)
        f.close()
		
        # Init task
        pygame.init()
        self.surface = screen
        self.surface.fill(background_color)
        self.words = words

        # Init objects
        self.indicator = list(range(len(words)))
        self.stimulus = Stimulus(screen)
        self.user_input = Input(screen)
        self.wait = Wait(screen)
        
    def zerofill(self, number, width):	
		width -= len(str(number))
		if (width > 0) :
			i = 0
			while i < width:
				number =   str(number).join("0")+ str(number)
				i += 1
		return number
        
    def write_data(self, word, starttyping, response, finishedword, time, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID)  + "_" + str(self.expStartTime) + "_alternate_uses_task"+".txt"
                    
        f = open(filename, 'a')
        output = str(self.expStartTime) + ";" + \
				 str(self.subjectID) + ";" + \
                 str(self.condition) + ";" + \
                 str(self.taskorder) + ";" + \
                 str(self.objectorder) + ";" + \
                 str(self.set_counter + 1) + ";" + \
                 str(word) + ";" + \
                 str(self.trialStartTime) + ";" + \
                 str(starttyping) + ";" + \
                 str(self.user_input.timeend) + ";" + \
                 str(self.numanswers) + ";" + \
                 str(response) + ";" + \
                 str(time) + ";" + \
                 str(finishedword) + "\n"
        f.write(output)
        f.close()
        
    def on_init(self):
        pygame.init()
        self.clock = pygame.time.Clock()
                                                     
    def on_loop(self, eachword):
		pygame.mouse.set_visible(False)
		self.stimulus.draw_word(eachword)
		
		self.user_input.draw_input(eachword, self.trialStartTime, self.trial_time)
		self._userinput = []
		
		if len(self.user_input.previous_string):
			self._userinput = self.user_input.previous_string
			self.user_input.previous_string = []
			self.numanswers += 1
			self.time = (self.user_input.timeend - self.user_input.startTyping)
			self.finishedword = "True"
			self.write_data(eachword, self.user_input.startTyping, self._userinput, self.finishedword, self.time)		
			self.user_input.startTyping = 0
			self.finishedword = "False"
			
    def on_render(self):	
		pygame.display.flip()
		
    def on_cleanup(self):
        pygame.quit()
        
    def run_trial(self, eachword, set_counter):
        self._running = True
        clock = pygame.time.Clock()
        self.trialStartTime = timer()
        while self._running:
			self.clock.tick(60)
			self.on_loop(eachword)
			self.on_render()
		
			if timer()-self.trialStartTime >= (self.trial_time/2.0):
				if len(self.user_input.current_string) > 0:
					noresponsetime = ""
					unfinishedword = "".join(self.user_input.current_string)
					self.finishedword = "False"
					self.user_input.timeend = ""
					self.write_data(eachword, self.user_input.startTyping, unfinishedword, self.finishedword, noresponsetime)
					self.user_input = Input(screen)
				else:
					noresponsetime = ""
					nowordentered = ""
					self.finishedword = "No input"
					self.user_input.startTyping = ""
					self.user_input.timeend = ""
					self.write_data(eachword, self.user_input.startTyping, nowordentered, self.finishedword, noresponsetime)
					self.user_input = Input(screen)
					
				self._running = False

    def on_execute(self):
		if self.on_init() == False:
			self._running = False

		intro_01 = pygame.image.load(os.path.join("images", "intro_AUT01.png")).convert()
		intro_02 = pygame.image.load(os.path.join("images", "intro_AUT02.png")).convert()
		self.wait.intro(intro_01)
		self.wait.intro(intro_02)

		for eachword in self.words:
			self.set_counter += 1
			self.numanswers = 0
			
			if self.set_counter + 1 == len(self.indicator):
				newobjectAUT = pygame.image.load(os.path.join("images", "intro_newobject_AUT01.png")).convert()
				self.wait.intro(newobjectAUT)
				
			self.run_trial(eachword, self.set_counter)
		
		if self.taskorder == "AUT_first":
			outro_01 = pygame.image.load(os.path.join("images", "outro_AUT&RAT.png")).convert()
		else:
			outro_01 = pygame.image.load(os.path.join("images", "general_outro_experiment.png")).convert()
			
		self.wait.intro(outro_01)	
		self.on_cleanup()

		
if __name__ == '__main__':
	debug = sys.argv[4]
	if debug == "f":
		trial_time = 600.0
	else:
		trial_time = 10.0
	
	expStartTime = sys.argv[1]
	subject_ID = sys.argv[2]
	if sys.argv[3] == "r":
		if int(subject_ID) % 2 == 1:
			condition = "c"
		else:
			condition = "d"
	else:
		condition = sys.argv[3]
	
	if int(subject_ID) % 4 == 1 or int(subject_ID) % 4 == 2:
		taskorder = "AUT_first"
	else:
		taskorder = "RAT_first"
	
	if int(subject_ID) % 8 == 3 or int(subject_ID) % 8 == 4 or int(subject_ID) % 8 == 5 or int(subject_ID) % 8 == 6:
		stimulus_set = ["pen", "krant"]
		objectorder = "pen_first"
	else:
		stimulus_set = ["krant", "pen"]
		objectorder = "krant_first"
		
	run = Main(stimulus_set, expStartTime, subject_ID, condition, taskorder, objectorder, trial_time)
	run.on_execute()
	
