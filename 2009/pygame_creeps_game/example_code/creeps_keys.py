import os, sys
import random
from math import sin, cos, radians
import pygame
from pygame.sprite import Sprite

sys.path.append('..')
from vec2d import vec2d


SCREEN_WIDTH, SCREEN_HEIGHT = 400, 400
BG_COLOR = 150, 150, 80
CREEP_FILENAME = '../images/bluecreep.png'


class Creep(Sprite):
    def __init__(   self, img_filename, 
                    init_position, init_direction, speed, screen):
        Sprite.__init__(self)
        
        self.screen = screen
        self.screen_rect = screen.get_rect()
        
        self.speed = speed
        self.base_image = pygame.image.load(img_filename).convert_alpha()
        self.image = self.base_image
        print self.image.get_at((0, 0))
        
        self.pos = init_position
        self.image_w, self.image_h = self.image.get_size()

        self.direction = init_direction.normalized()
        self.counter = 0

    def _update_direction(self, time_passed):
        self.counter += time_passed
        if self.counter > 500:
            self.direction.rotate(45 * random.randint(-1, 1))
            self.counter -= 500
            
    def update(self, time_passed):
        self._update_direction(time_passed)
        
        self.image = pygame.transform.rotate(self.base_image, -self.direction.angle)
        self.image_w, self.image_h = self.image.get_size()
        
        velocity = vec2d(    
            self.direction.x * self.speed * time_passed,
            self.direction.y * self.speed * time_passed)
        
        self.pos += velocity
        
        if self.pos.x < self.screen_rect.left + self.image_w / 2 + 1:
            self.pos.x = self.screen_rect.left + self.image_w / 2 + 2
            self.direction = self.direction.perpendicular()
        elif self.pos.x > self.screen_rect.right - self.image_w / 2 - 1:
            self.pos.x = self.screen_rect.right - self.image_w / 2 - 2
            self.direction.rotate(90)
        elif self.pos.y < self.screen_rect.top + self.image_h / 2 + 1:
            self.pos.y = self.screen_rect.top + self.image_h / 2 + 2
            self.direction = self.direction.perpendicular()
        elif self.pos.y > self.screen_rect.bottom - self.image_h / 2 - 1:
            self.pos.y = self.screen_rect.bottom - self.image_h / 2 - 2
            self.direction.rotate(90)
        
        self.rect = self.image.get_rect()
        self.rect.topleft = (self.pos.x, self.pos.y)
    
    def blitme(self):
        draw_pos = self.rect.move(
            [-self.image_w / 2, -self.image_h / 2])
        self.screen.blit(self.image, draw_pos)
        

def run_game():
    pygame.init()
    screen = pygame.display.set_mode(
                (SCREEN_WIDTH, SCREEN_HEIGHT), 0, 32)
    clock = pygame.time.Clock()
    
    creep = pygame.image.load(CREEP_FILENAME).convert_alpha()
    autocreep = Creep(
        CREEP_FILENAME, vec2d(300, 100), vec2d(1, 1), 0.1, screen)
    
    creep_pos = vec2d(220, 200)
    creep_speed = 0.09 # px per ms
    creep_dir = vec2d(-1, -1).normalized()

    while True:
        time_passed = clock.tick(35)
        
        movement_dir = 0
        rotation_dir = 0
        
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                exit_game()
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    rotation_dir = -1
                elif event.key == pygame.K_RIGHT:
                    rotation_dir = 1
        
        pressed_keys = pygame.key.get_pressed()
        
        if pressed_keys[pygame.K_UP]:
            movement_dir = 1
        elif pressed_keys[pygame.K_DOWN]:
            movement_dir = -1

        creep_dir.rotate(45 * rotation_dir)
        
        creep_pos += creep_dir * movement_dir * creep_speed * time_passed

        rotated_creep = pygame.transform.rotate(creep, -creep_dir.angle)
        w, h = rotated_creep.get_size()
        creep_draw_pos = vec2d( creep_pos.x - w / 2, 
                                creep_pos.y - h / 2)
        
        screen.fill(BG_COLOR)
        
        autocreep.update(time_passed)
        autocreep.blitme()
        #~ screen.blit(autocreep.image, autocreep.rect)
        
        
        screen.blit(rotated_creep, creep_draw_pos)

        pygame.display.flip()


def exit_game():
    sys.exit()


run_game()
