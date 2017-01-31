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
button_edge_color = (0, 0, 0)
button_back_color = (200, 200, 200)
button_text_color = (40, 40, 40)
incorrect_color = (255, 0, 40)
correct_color = (50, 255, 0)
counter_text_color = (50, 50, 50)
box_width = 250
box_height = 60
button_width = 150
button_height = 35
screen_w = 640
screen_h = 500
fps = 60
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
        self.total_repeat_words = 0
        self.total_correct_words = 0
        self.total_incorrect_words = 0
        self.n_repeat_words = 0
        self.font = pygame.font.Font(None, 50)
        self.font_text = pygame.font.Font(None, 20)
        self.font_feedback = pygame.font.Font(None, 40)
        self.font_counter = pygame.font.Font(None, 40)

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
        self.draw_counter()

        pygame.display.flip()

    def draw_counter(self):
        pygame.draw.rect(self.surface, (255, 255, 255),
                         ((self.x - 320), self.y + 155,
                          640, 100), 0)

        countertext_1 = "Probeer de taak door bijvoorbeeld, 'erwt', 'drie' en 'wiedt' te typen."
        text = self.font_text.render(countertext_1, 1, counter_text_color)
        self.surface.blit(text, (self.x - 220, self.y + 155))
        countertext_2 = "Probeer ook een verkeerd woord zoals 'markt' of een dubbel woord (nog een keer 'erwt')."
        text = self.font_text.render(countertext_2, 1, counter_text_color)
        self.surface.blit(text, (self.x - 280, self.y + 180))
        countertext_3 = "Druk op enter om het woord in te voeren"
        text = self.font_text.render(countertext_3, 1, counter_text_color)
        self.surface.blit(text, (self.x - 125, self.y + 205))

    def draw_input(self, correct):
        self.text = self.font_counter.render("Score: " + str(self.total_correct_words), 1, color_font)
        self.textpos = self.text.get_rect(topleft=(10, 10))
        self.surface.blit(self.text, self.textpos)
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_BACKSPACE:
                    self.current_string = self.current_string[:-1]
                elif event.key == pygame.K_RETURN:
                    self.checker(string.join(self.current_string, ""),
                                 correct)
                    if self.n_correct_words >= 5:
                        break
                    self.current_string = []
                elif event.key <= 127:
                    self.current_string.append(chr(event.key))
            elif event.type == pygame.MOUSEBUTTONUP:
                self.prev_n_correct = self.n_correct_words
                self.prev_n_incorrect = self.n_incorrect_words
                self.n_correct_words = 0
                self.n_incorrect_words = 0
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
            self.n_repeat_words += 1
            self.total_repeat_words += 1
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
            pygame.draw.rect(self.surface, (255, 255, 255), ((self.x - 200), self.y - 80,400, 60), 0)
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

    def waiter(self, time=15000):
        self.surface.fill(background_color)
        text = self.font.render("Wacht op de volgende letterset", 1,
                                color_font)
        self.surface.blit(text, (self.x - 140, self.y))
        pygame.display.flip()
        pygame.time.delay(time)
        self.surface.fill(background_color)

    def outro(self):
        self.surface.fill(background_color)
        text = self.font.render("Dat was de oefening voor de scrabbletaak!",
                                1, color_font)
        self.surface.blit(text, (self.x - 180, self.y - 50))
        text = self.font.render("In het volgende scherm begint de eerste scrabbletaak.",
                                1, color_font)
        self.surface.blit(text, (self.x - 225, self.y - 15))
        text = self.font.render("Druk op de spatiebalk om verder te gaan",
                                1, color_font)
        self.surface.blit(text, (self.x - 170, self.y + 50))

        pygame.display.flip()
        while True:
            event = pygame.event.poll()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    return


class Button:
    def __init__(self, surface):
        self.surface = surface
        self.x = surface.get_width() / 2
        self.y = surface.get_height() / 2
        self.font = pygame.font.Font(None, 30)

    def stop_practice(self):
        pygame.draw.rect(self.surface, button_back_color,
                         ((self.x - (button_width / 2)), self.y + 100,
                          button_width, button_height), 0)
        pygame.draw.rect(self.surface, button_edge_color,
                         ((self.x - (button_width / 2)), self.y + 100,
                          button_width, button_height), 1)
        text = self.font.render("stop oefenen", 1, color_font)
        screen.blit(text, (self.x - button_width / 2 + 12, self.y + 105))
        pygame.display.flip()


class Main:
    def __init__(self, letters, words):
        # Init task
        pygame.init()
        self.surface = screen
        self.surface.fill(background_color)
        self.letters = letters
        self.word = words

        # Init objects
        self.stimulus = Stimulus(screen)
        self.user_input = Input(screen)
        self.wait = Wait(screen)
        self.button = Button(screen)

    def main(self):
        # Main loop
        self.start = timer()
        clock = pygame.time.Clock()
        image_general_intro = pygame.image.load(os.path.join("images", "general_intro_experiment.png")).convert()
        image_intro01 = pygame.image.load(os.path.join("images", "intro_scrabble_practice01.png")).convert()
        image_intro02 = pygame.image.load(os.path.join("images", "intro_scrabble_practice02.png")).convert()
        image_intro03 = pygame.image.load(os.path.join("images", "intro_scrabble_practice03.png")).convert()
        self.begin = timer()

        self.wait.intro(image_general_intro)
        self.wait.intro(image_intro01)
        self.wait.intro(image_intro02)
        self.wait.intro(image_intro03)
        self.stimulus.draw_letterset(self.letters)
        self.button.stop_practice()
        self.user_input.draw_input(self.word)
        self.time = (timer() - self.begin)
        
        self.end = timer()
        screen.fill(background_color)
        clock.tick(fps)

if __name__ == '__main__':
    stimulus_set = "I E D T W R"
    correct_words = ["weid", "wedt", "werd", "wied", "wiet", "erwt", "wier", 
                     "tred", "tier", "rite", "drie", "redt", "riet", "ried", 
                     "dier","weidt","wiedt"]

    run = Main(stimulus_set, correct_words)
    run.main()
