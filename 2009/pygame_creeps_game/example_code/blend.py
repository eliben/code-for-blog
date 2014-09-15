import pygame
from pygame.locals import *

TRANSPARENT = (255,0,255)
pygame.init()
screen = pygame.display.set_mode((500,500))

surf1 = pygame.Surface((200,200))
surf1.fill(TRANSPARENT)
surf1.set_colorkey(TRANSPARENT)
pygame.draw.circle(surf1, (0,0,200,100),(100,100), 100)

surf2 = pygame.Surface((200,200))
surf2.fill(TRANSPARENT)
surf2.set_colorkey(TRANSPARENT)
pygame.draw.circle(surf2, (200,0,0,100),(100,100), 100)

surf1.set_alpha(100)
surf2.set_alpha(100)

                    
srect = pygame.Surface((90, 90))
pygame.draw.rect(srect, Color(0, 120, 200), Rect(0, 0, 90, 90))
srect.set_alpha(130)


while True:
    screen.fill((255,255,255))

    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()

    screen.blit(surf1, (100,100,100,100))
    screen.blit(surf2, (200,200,100,100))
    screen.blit(srect, (350, 250))
    pygame.display.flip()
