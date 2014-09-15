import sys
import pygame
from pygame import Rect


def draw_background(screen, tile_img_file, field_rect):
    tile_img = pygame.image.load(tile_img_file).convert_alpha()
    img_rect = tile_img.get_rect()
    
    nrows = int(screen.get_height() / img_rect.height) + 1
    ncols = int(screen.get_width() / img_rect.width) + 1
    
    for y in range(nrows):
        for x in range(ncols):
            img_rect.topleft = (x * img_rect.width, 
                                y * img_rect.height)
            screen.blit(tile_img, img_rect)
    
    field_color = (109, 41, 1)
    boundary_rect = Rect(   field_rect.left - 4, 
                            field_rect.top - 4, 
                            field_rect.width + 8, 
                            field_rect.height + 8)
    pygame.draw.rect(screen, (0, 0, 0), boundary_rect)
    pygame.draw.rect(screen, field_color, field_rect)
    
    
def run_game():
    # Game parameters
    SCREEN_WIDTH, SCREEN_HEIGHT = 400, 400
    FIELD_RECT = Rect(50, 50, 300, 300)
    BG_TILE_IMG = '../images/brick_tile.png'
    
    
    pygame.init()
    screen = pygame.display.set_mode(
                (SCREEN_WIDTH, SCREEN_HEIGHT), 0, 32)
    clock = pygame.time.Clock()

    while True:
        time_passed = clock.tick(30)
        
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                exit_game()

        draw_background(screen, BG_TILE_IMG, FIELD_RECT)
        pygame.display.flip()


def exit_game():
    sys.exit()


run_game()
