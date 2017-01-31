#! /usr/bin/env python

import pygame
import string
import os
import sys
import random
import csv
from timeit import default_timer as timer

# Constants
color_font = (30, 30, 30)
background_color = (255, 255, 255)
box_edge_color = (0, 0, 0)
box_back_color = (240, 240, 240)
box_text_color = (100, 100, 100)
box_width = 250
box_height = 60

screen_w = 640
screen_h = 500
screen = pygame.display.set_mode((screen_w, screen_h), pygame.HWSURFACE |
                                 pygame.DOUBLEBUF | pygame.FULLSCREEN)

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
        self.font = pygame.font.SysFont("couriernew", 35)
        self.font_text = pygame.font.Font(None, 20)

    def draw_text_box(self, message):
        pygame.draw.rect(self.surface, box_back_color,
                         ((self.x - (box_width / 2)), self.y,
                          box_width, box_height), 0)
        pygame.draw.rect(self.surface, box_edge_color,
                         ((self.x - (box_width / 2)), self.y,
                          box_width, box_height), 1)
                                            
        if len(message) != 0:            
            self.surface.blit(self.font.render(message, 1, box_text_color),
										(self.x - 100, self.y + 10))
        pygame.display.flip()

    def draw_input(self, eachset):
		event = pygame.event.poll()
		
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
	
    def choicewait(self, image):
        self.surface.fill(background_color)
        self.surface.blit(image, (50, 50))

        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_1:
					pressedkey = 1
					pressedkey_time = timer()
					return(pressedkey, pressedkey_time)
                elif event.key == pygame.K_2:
					pressedkey = 2
					pressedkey_time = timer()
					return(pressedkey, pressedkey_time)
                elif event.key == pygame.K_3:
					pressedkey = 3
					pressedkey_time = timer()
					return(pressedkey, pressedkey_time)	
	
class Main:
    def __init__(self, stimulus_set, expStartTime, subjectID, condition, taskorder, trial_time):
		# Init data collection
        self._running = True
        self.start_time = timer()
        self.expStartTime = expStartTime
        self.subjectID = subjectID
        self.condition = condition
        self.participant_input = []
        self.trial_time = trial_time
        self.set_counter = -1
        self.response = 0
        self.finishedword = "False"
        self.taskorder = taskorder
        self.time = 0

        self.subjectID = self.zerofill(self.subjectID, 4)
        filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_remote_association_task"+".txt"
        f = open(filename, 'w')
        output = 'expStartTime;subjectID;condition;taskorder;nth_set;stimulusset;trialStart;time_start_set;time_end_setresponse;' \
                 'setresponse;setresponsetime;setfinishedresponse;time_start_option;time_end_optionresponse;optionresponse;optionresponsetime\n'
        f.write(output)
        f.close()
		
        # Init task
        pygame.init()
        self.surface = screen
        self.surface.fill(background_color)
        self.stimulus_set = stimulus_set
        random.shuffle(self.stimulus_set)

        # Init objects
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
        
    def write_data(self, stimulus_set, setresponse, finishedword, time, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID)  + "_" + str(self.expStartTime) + "_remote_association_task"+".txt"
        f = open(filename, 'a')
        output = str(self.expStartTime) + ";" + \
				 str(self.subjectID) + ";" + \
                 str(self.condition) + ";" + \
                 str(self.taskorder) + ";" + \
                 str(self.set_counter + 1) + ";" + \
                 str(stimulus_set) + ";" + \
                 str(self.trialStartTime) + ";" + \
                 str(self.begin) + ";" + \
                 str(self.user_input.timeend) + ";" + \
                 str(setresponse) + ";" + \
                 str(time) + ";" + \
                 str(finishedword) + ";" #+  "\n"
        f.write(output)
        f.close()
        
    def extend_data(self, buttonclicked, timeend, time, filename=None):
		if filename is None:
			filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID)  + "_" + str(self.expStartTime) + "_remote_association_task"+".txt"
		f = open(filename, 'a')
		output = str(self.begin) + ";" + \
				 str(timeend) + ";" + \
				 str(buttonclicked) + ";" + \
				 str(time) + "\n"
		f.write(output)
		f.close()
       
    def on_init(self):
        pygame.init()
        self.clock = pygame.time.Clock()
                                                     
    def on_loop(self, eachset):
		pygame.mouse.set_visible(False)
		self.stimulus.draw_word(eachset)
		self.user_input.draw_input(eachset)
		self._userinput = []
		
		if len(self.user_input.previous_string):
			self._userinput = self.user_input.previous_string
			self.user_input.previous_string = []
			self.time = (self.user_input.timeend - self.begin)
			self.finishedword = "True"
			self.write_data(eachset, self._userinput, self.finishedword, self.time)		
			self.begin = timer()
			self.finishedword = "False"
			self._running = False
			
    def on_render(self):	
		pygame.display.flip()
		
    def on_cleanup(self):
        pygame.quit()
        
    def run_trial(self, eachset, set_counter):
        self._running = True
        clock = pygame.time.Clock()
        self.trialStartTime = timer()
        self.begin = timer()
        while self._running:
			self.clock.tick(60)
			self.on_loop(eachset)
			self.on_render()
			
			if timer()-self.trialStartTime >= self.trial_time:
				if len(self.user_input.current_string) > 0:
					noresponsetime = ""
					unfinishedword = "".join(self.user_input.current_string)
					self.finishedword = "False"
					self.user_input.timeend = ""
					self.write_data(eachset, unfinishedword, self.finishedword, noresponsetime)
					self.user_input = Input(screen)
				else:
					noresponsetime = ""
					nowordentered = ""
					self.finishedword = "No input"
					self.user_input.timeend = ""
					self.write_data(eachset, nowordentered, self.finishedword, noresponsetime)
					self.user_input = Input(screen)
					
				self._running = False
        
        #Present choice option here
        choiceimage = pygame.image.load(os.path.join("images", "choice_RAT.png")).convert()
        self.begin = timer()
        self.response = self.wait.choicewait(choiceimage)
        self.time = (self.response[1] - self.begin)
        self.extend_data(self.response[0], self.response[1], self.time)


    def on_execute(self):
		if self.on_init() == False:
			self._running = False
		
		intro_01 = pygame.image.load(os.path.join("images", "intro_RAT01.png")).convert()
		intro_02 = pygame.image.load(os.path.join("images", "intro_RAT02.png")).convert()
		intro_03 = pygame.image.load(os.path.join("images", "intro_RAT03.png")).convert()
		self.wait.intro(intro_01)
		self.wait.intro(intro_02)
					
		self.run_trial("kuip / zwem / jas", self.set_counter)
		self.wait.intro(intro_03)
			
		for eachset in self.stimulus_set:
			self.set_counter += 1
			
			self.surface.fill(background_color)
			self.surface.blit(self.user_input.font.render("+", 1, box_text_color),
										(self.surface.get_width() / 2,self.surface.get_height() / 2))		
			pygame.display.flip()
			pygame.time.delay(1000)
			self.run_trial(eachset, self.set_counter)
		
		if self.taskorder == "AUT_first":
			outro_01 = pygame.image.load(os.path.join("images", "general_outro_experiment.png")).convert()
		else:
			outro_01 = pygame.image.load(os.path.join("images", "outro_AUT&RAT.png")).convert()	
			
		self.wait.intro(outro_01)
		self.on_cleanup()

		
if __name__ == '__main__':
	debug = sys.argv[4]
	if debug == "f":
		trial_time = 20.0
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
	
	if int(subject_ID) % 4 == 1 or int(subject_ID) % 4 == 2:
		taskorder = "AUT_first"
	else:
		taskorder = "RAT_first"
	
	# Read letter set file
	stimulus_set = []
	with open(os.path.join('csv_lettersets', 'RATwords.csv'), 'rb') as csvfile:
		sets = csv.reader(csvfile, delimiter=' ')
		for row in sets:
			stimulus_set.append(' '.join(row))
	
	run = Main(stimulus_set, expStartTime, subject_ID, condition, taskorder, trial_time)
	run.on_execute()
	
