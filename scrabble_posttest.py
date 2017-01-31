#! /usr/bin/env python

import pygame
import string
import random
import csv
import os
import sys
from timeit import default_timer as timer

# Constants
color_font = (30, 30, 30)
background_color = (255, 255, 255)
box_edge_color = (0, 0, 0)
box_back_color = (240, 240, 240)
box_text_color = (100, 100, 100)
button_edge_color = (0, 0, 0)
button_back_color = (200, 200, 200)
button_text_color = (40, 40, 40)
incorrect_color = (255, 0, 40)
correct_color = (50, 255, 0)
counter_text_color = (150, 150, 150)
box_width = 250
box_height = 60
button_width = 150
button_height = 35
screen_w = 640
screen_h = 500
fps = 60
screen = pygame.display.set_mode((screen_w, screen_h), pygame.HWSURFACE |
                                 pygame.DOUBLEBUF | pygame.FULLSCREEN)


class Stimulus:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.font = pygame.font.Font(None, 60)

    def draw_letterset(self, letters):
        self.surface.fill(background_color)
        text = self.font.render(letters, 1, color_font)
        self.surface.blit(text, (self.x - 110, self.y - 150))


class Input:
    """This class takes care of user input"""
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.current_string = []
        self.previous_string = ""
        self.past_correct_words = []
        self.past_incorrect_words = []
        self.n_correct_words = 0
        self.n_incorrect_words = 0
        self.prev_n_correct = 0
        self.prev_n_incorrect = 0
        self.total_correct_words = 0
        self.total_incorrect_words = 0
        self.font = pygame.font.Font(None, 50)
        self.font_feedback = pygame.font.Font(None, 40)
        self.font_counter = pygame.font.Font(None, 40)
        self.running = True

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

    def draw_input(self, correct):
        self.text = self.font_counter.render("Score: " + str(self.total_correct_words), 1, color_font)
        self.textpos = self.text.get_rect(topleft=(10, 10))
        self.surface.blit(self.text, self.textpos)
        
        """Draw user input"""
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_BACKSPACE:
                    self.current_string = self.current_string[:-1]
                elif event.key == pygame.K_RETURN:
                    self.checker(string.join(self.current_string, ""),
                                 correct)
                    self.current_string = []
                    if self.total_correct_words >= 30:
                        self.running = False
                        break
                elif event.key <= 127:
                    self.current_string.append(chr(event.key))
            elif event.type == pygame.MOUSEBUTTONUP:
                self.prev_n_correct = self.n_correct_words
                self.prev_n_incorrect = self.n_incorrect_words
                self.n_correct_words = 0
                self.n_incorrect_words = 0
                self.current_string = []
                if self.x - (button_width / 2) <= event.pos[0] <= \
                   self.x + (button_width / 2) and \
                   self.y + 100 <= event.pos[1] <= self.y + 100 + \
                   button_height:
                    break

            self.draw_text_box(string.join(self.current_string, ""))

    def checker(self, word, cor_words):
        correct = cor_words
        word = word
        # check whether this word has been entered earlier
        if word in self.past_correct_words:
            self.surface.blit(self.font_feedback.render("Dit woord heb je "
                                                        "al gehad",
                              1, incorrect_color),
                              (self.x - 160, self.y - 70))
            pygame.display.flip()
            pygame.time.delay(800)
            pygame.draw.rect(self.surface, (255, 255, 255),
                             ((self.x - 200), self.y - 80,
                              400, 60), 0)
            # check whether this word is in the correct words list
        elif word in correct:
            self.surface.blit(self.font_feedback.render("Correct!",
                              1, correct_color),
                              (self.x - 55, self.y - 70))
            self.past_correct_words.append(word)
            self.n_correct_words += 1
            self.total_correct_words += 1
            pygame.display.flip()
            
            pygame.time.delay(800)
            pygame.draw.rect(self.surface, (255, 255, 255), (5, 5, 140, 30), 0)
            pygame.draw.rect(self.surface, (255, 255, 255),((self.x - 200), self.y - 80,400, 60), 0)
            self.text = self.font_counter.render("Score: " + str(self.total_correct_words), 1, color_font)
            self.surface.blit(self.text, self.textpos)
            
            # else this is not a correct word
        else:
            self.surface.blit(self.font_feedback.render("Incorrect",
                              1, incorrect_color),
                              (self.x - 60, self.y - 70))
            self.past_incorrect_words.append(word)
            self.n_incorrect_words += 1
            self.total_incorrect_words += 1
            pygame.display.flip()
            pygame.time.delay(800)
            pygame.draw.rect(self.surface, (255, 255, 255),
                             ((self.x - 200), self.y - 80,
                              400, 60), 0)


class Wait:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.font = pygame.font.Font(None, 30)

    def intro(self, image):
        self.surface.fill(background_color)
        self.surface.blit(image, (50, 50))

        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    return

    def experimenter(self, image):
        self.surface.fill(background_color)
        self.surface.blit(image, (50, 50))

        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_PAGEDOWN:
                    return

    def waiter(self, time):
        self.surface.fill(background_color)
        text = self.font.render("Wacht op de volgende letterset", 1,
                                color_font)
        self.surface.blit(text, (self.x - 140, self.y))
        pygame.display.flip()
        pygame.time.delay(time)
        self.surface.fill(background_color)


class Button:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.font = pygame.font.Font(None, 30)

    def next_set(self):
        pygame.draw.rect(self.surface, button_back_color,
                         ((self.x - (button_width / 2)), self.y + 100,
                          button_width, button_height), 0)
        pygame.draw.rect(self.surface, button_edge_color,
                         ((self.x - (button_width / 2)), self.y + 100,
                          button_width, button_height), 1)
        text = self.font.render("volgende set", 1, color_font)
        screen.blit(text, (self.x - button_width / 2 + 12, self.y + 105))
        pygame.display.flip()


class Main:
    def __init__(self, letters, words, expStartTime, subjectID, condition, wait_time):
        # Init data collection
        self.start_time = timer()
        self.expStartTime = expStartTime
        self.subjectID = subjectID
        self.condition = condition
        self.total_correct_n = 0
        self.total_incorrect_n = 0
        self.correct_input = []
        self.incorrect_input = []
        self.set_counter = -1
        self.wait_time = wait_time
		
        self.subjectID = self.zerofill(self.subjectID, 4)
        filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_scrabble_posttest"+".txt"
        f = open(filename, 'w')
        output = 'expStartTime;subjectID;condition;nth_set;letterset;time_start;time_end;' \
                 'time_in_set;correct_n;correct_words;incorrect_n;' \
                 'incorrect_words\n'
        f.write(output)
        f.close()

        # Init task
        pygame.init()
        self.surface = screen
        self.surface.fill(background_color)
        self.letters = letters
        self.word = words

        # Initiate objects
        self.indicator = list(range(len(stimulus_set)))
        random.shuffle(self.indicator)
        self.stimulus = Stimulus(screen)
        self.user_input = Input(screen)
        self.wait = Wait(screen)
        self.button = Button(screen)

    def main(self):
        # Main loop
        clock = pygame.time.Clock()
        image_intro01 = pygame.image.load(os.path.join("images", "intro_scrabble_posttest01.png")).convert()
        self.wait.intro(image_intro01)

        for number in self.indicator:
            """Loop through letter sets and check input until next set
            button is clicked. Repeat until last set has been shown"""
            self.begin = timer()
            self.set_counter += 1
            self.stimulus.draw_letterset(self.letters[number])
            self.button.next_set()

            self.user_input.draw_input(correct_words[number])
            self.correct_input.append(self.user_input.past_correct_words)
            self.incorrect_input.append(self.user_input.past_incorrect_words)
            self.time = (timer() - self.begin)
            self.write_data(number)

            self.user_input.n_correct_words = 0
            self.user_input.n_incorrect_words = 0
            self.user_input.past_correct_words = []
            self.user_input.past_incorrect_words = []

            if not self.user_input.running:
				image_outro01 = pygame.image.load(os.path.join("images", "outro_scrabble_posttest01.png")).convert()
				image_outro02 = pygame.image.load(os.path.join("images", "intro_AUT&RAT.png")).convert()
				self.wait.intro(image_outro01)
				self.wait.experimenter(image_outro02)
				
				return
            elif self.set_counter + 1 == len(self.indicator):  # Check if this was the final set
				image_outro01 = pygame.image.load(os.path.join("images", "outro_scrabble_posttest01.png")).convert()
				image_outro02 = pygame.image.load(os.path.join("images", "intro_AUT&RAT.png")).convert()
				self.wait.intro(image_outro01)
				self.wait.experimenter(image_outro02)
				
            else:
                self.wait.waiter(time=self.wait_time)

        screen.fill(background_color)
        clock.tick(fps)

    def zerofill(self, number, width):	
		width -= len(str(number))
		if (width > 0) :
			i = 0
			while i < width:
				number =   str(number).join("0")+ str(number)
				i += 1
		return number

    def write_data(self, number, filename=None):
        if filename is None:
            filename = os.path.join("output", "ERC_WP3_Year1_Study1_") + str(self.subjectID) + "_" + str(self.expStartTime) + "_scrabble_posttest"+".txt"
        f = open(filename, 'a')
        output = str(self.expStartTime) + ";" + \
				 str(self.subjectID) + ";" + \
                 str(self.condition) + ";" + \
                 str(self.set_counter + 1) + ";" + \
                 str(self.letters[number]) + ";" + \
                 str(self.begin) + ";" + \
                 str(timer()) + ";" + \
                 str(self.time) + ";" + \
                 str(self.user_input.prev_n_correct) + ";" + \
                 str(self.correct_input[self.set_counter]) + ";" + \
                 str(self.user_input.prev_n_incorrect) + ";" + \
                 str(self.incorrect_input[self.set_counter]) + "\n"
        f.write(output)
        f.close()

if __name__ == '__main__':
    debug = sys.argv[4]
    if debug == "f":
        wait_time = 15000
    else:
		wait_time = 100
	
    expStartTime = sys.argv[1]
    subject_ID = sys.argv[2]
    if sys.argv[3] == "r":
		if int(subject_ID) % 2 == 1:
			condition = "c"
		else:
			condition = "d"
    else:
		condition = sys.argv[3]

    # Read letter set file
    stimulus_set = []
    with open(os.path.join('csv_lettersets', 'lettersets_posttest.csv'), 'rb') as csvfile:
        letters = csv.reader(csvfile, delimiter=' ')
        for row in letters:
            stimulus_set.append(' '.join(row))

    # Read correct words file
    correct_words = []
    with open(os.path.join('csv_lettersets', 'words_posttest.csv'), 'rU') as f:
        reader = csv.reader(f, delimiter=' ')
        for row in reader:
            correct_words.append(' '.join(row))
    for i in range(len(correct_words)):
        correct_words[i] = correct_words[i].split(',')

    run = Main(stimulus_set, correct_words, expStartTime, subject_ID, condition, wait_time)
    run.main()
