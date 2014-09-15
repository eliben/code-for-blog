import sys
sys.path.append('..')

import time
import pygame
from pygame import Rect, Color

from pathfinder import PathFinder
from gridmap import GridMap


class Visualizer(object):
    def __init__(self, screen, field, message_func):
        self.screen = screen
        self.field = field
        self.message_func = message_func
        
        self.grid_size = 15
        
        self.field_color = Color('black')
        self.grid_color = Color('gray')
        self.start_pos_color = Color('red')
        self.goal_pos_color = Color('green')
        self.path_color = Color('violet')
        self.blocked_color = Color('gray')
        
        self._init_map()
        
    def draw(self):
        self._draw_grid(self.field)
        self._draw_map(self.field, 
            self.blocked_list, self.start_pos,
            self.goal_pos, self.path)
        
        self.message_func(self.msg1, self.msg2)
    
    def user_event(self, event):
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_F5:
                self._recompute_path()
        elif event.type == pygame.MOUSEBUTTONDOWN:
            self.path_valid = False
            self.msg1 = 'Please recompute path (F5)'
            self.msg2 = ''
            
            self._handle_mouse_click(event)
    
    ########################## PRIVATE ##########################

    def _init_map(self):
        self.start_pos = 0, 0
        self.goal_pos = 3, 8
        
        nrows = self.field.height / self.grid_size
        ncols = self.field.width / self.grid_size
        
        self.map = GridMap(nrows, ncols)
        for b in [  (1, 1), (1, 2), (0, 3), (1, 3), (2, 3), 
                    (2, 4), (2, 5), (2, 6)]:
            self.map.set_blocked(b)
                    
        self._recompute_path()
    
    def _handle_mouse_click(self, event):
        if not self.field.collidepoint(event.pos):
            return
        
        ncol = (event.pos[0] - self.field.left) / self.grid_size
        nrow = (event.pos[1] - self.field.top) / self.grid_size
        coord = (nrow, ncol)

        if event.button == 1:
            self.map.set_blocked(coord, not self.map.blocked[coord])
        elif event.button == 2:
            self.start_pos = coord
        elif event.button == 3:
            self.goal_pos = coord
        
    def _recompute_path(self):
        self.blocked_list = self.map.blocked
        
        pf = PathFinder(self.map.successors, self.map.move_cost, 
                self.map.move_cost)
        
        t = time.clock()
        self.path = list(pf.compute_path(self.start_pos, self.goal_pos))
        dt = time.clock() - t
        
        if self.path == []:
            self.msg1 = "No path found"
        else:
            self.msg1 = "Found path (length %d)" % len(self.path)
        
        self.msg2 = "Elapsed: %s seconds" % dt
        self.path_valid = True

    def _draw_grid(self, field):
        """ Draw a grid on the given surface.
        """
        self.screen.fill(self.field_color, field)
        
        nrows = field.height / self.grid_size
        ncols = field.width / self.grid_size
        
        for y in range(nrows + 1):
            pygame.draw.line(
                self.screen,
                self.grid_color,
                (field.left, field.top + y * self.grid_size - 1),
                (field.right - 1, field.top + y * self.grid_size - 1))
        
        for x in range(ncols + 1):
            pygame.draw.line(
                self.screen,
                self.grid_color,
                (field.left + x * self.grid_size - 1, field.top),
                (field.left + x * self.grid_size - 1, field.bottom - 1))

    def _draw_map(self, field, blocked, start, goal, path):
        def _fill_square((nrow, ncol), color):
            left = field.left + ncol * self.grid_size 
            top = field.top + nrow * self.grid_size
            width = self.grid_size - 1
            
            self.screen.fill(color, Rect(left, top, width, width))
        
        def _fill_spot((nrow, ncol), color):
            pos_x = field.left + ncol * self.grid_size + self.grid_size / 2
            pos_y = field.top + nrow * self.grid_size + self.grid_size / 2
            radius = self.grid_size / 4
            
            pygame.draw.circle(self.screen, 
                color, (pos_x, pos_y), radius)
        
        for bl in blocked:
            _fill_square(bl, self.blocked_color)
        
        if self.path_valid:
            for path_square in path:
                _fill_spot(path_square, self.path_color)

        _fill_spot(start, self.start_pos_color)
        _fill_spot(goal, self.goal_pos_color)
    

def draw_messages(screen, rect, message1, message2):
    draw_rimmed_box(screen, rect, (50, 20, 0), 4, Color('white'))
    
    my_font = pygame.font.SysFont('arial', 18)
    message1_sf = my_font.render(message1, True, Color('white'))
    message2_sf = my_font.render(message2, True, Color('white'))
    
    screen.blit(message1_sf, rect.move(10, 0))
    screen.blit(message2_sf, rect.move(10, message1_sf.get_height()))


def draw_rimmed_box(screen, box_rect, box_color, 
                    rim_width=0, 
                    rim_color=Color('black')):
    """ Draw a rimmed box on the given surface. The rim is drawn
        outside the box rect.
    """
    if rim_width:
        rim_rect = Rect(box_rect.left - rim_width,
                        box_rect.top - rim_width,
                        box_rect.width + rim_width * 2,
                        box_rect.height + rim_width * 2)
        pygame.draw.rect(screen, rim_color, rim_rect)
    
    pygame.draw.rect(screen, box_color, box_rect)


def draw_title(screen, rect):
    draw_rimmed_box(screen, rect, (40, 10, 60), 4, Color('gray'))
    
    msgs = [
        'Left click to toggle wall',
        'Middle click to set start (red)',
        'Right click to set goal (green)',
        'F5 to recompute the path',
    ]
    
    my_font = pygame.font.SysFont('arial', 16)
    
    for i, msg in enumerate(msgs):
        rendered = my_font.render(msg, True, Color('white'))
        screen.blit(rendered, rect.move(10, i * rendered.get_height()))
    

def run_game():
    SCREEN_WIDTH, SCREEN_HEIGHT = 350, 550
    FIELD_RECT = Rect(25, 130, 300, 300)
    MESSAGES_RECT = Rect(25, 450, 300, 50)
    TITLE_RECT = Rect(25, 10, 300, 90)
    
    pygame.init()
    screen = pygame.display.set_mode(
                (SCREEN_WIDTH, SCREEN_HEIGHT), 0, 32)
    clock = pygame.time.Clock()
    
    def message_func(msg1, msg2):
        draw_messages(screen, MESSAGES_RECT, msg1, msg2)
    
    visualizer = Visualizer(screen, FIELD_RECT, message_func)
    
    while True:
        time_passed = clock.tick(30)
        
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                exit_game()
            else:
                visualizer.user_event(event)
   
        draw_title(screen, TITLE_RECT)
    
        visualizer.draw()
       
        pygame.display.flip()


def exit_game():
    sys.exit()


run_game()

