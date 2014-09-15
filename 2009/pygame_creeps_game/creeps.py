import os, sys
from random import randint, choice
from math import sin, cos, radians

import pygame
from pygame import Rect, Color
from pygame.sprite import Sprite

from gridmap import GridMap
from pathfinder import PathFinder
from simpleanimation import SimpleAnimation
from utils import Timer
from vec2d import vec2d
from widgets import Box, MessageBoard


class Creep(Sprite):
    """ A creep sprite that bounces off walls and changes its
        direction from time to time.
    """
    def __init__(   
            self, screen, game, creep_images, explosion_images, 
            field, init_position, init_direction, speed):
        """ Create a new Creep.
        
            screen: 
                The screen on which the creep lives (must be a 
                pygame Surface object, such as pygame.display)
            
            game:
                The game object that holds information about the
                game world.
            
            creep_images: 
                A pair of images (as Pygame surface objects) for 
                the creep. The first one should point at 3 
                o'clock, and the second diagonally between 12
                and 3 o'clock (at 45 degrees above the horizontal
                plane)
            
            explosion_images:
                A list of image objects for the explosion 
                animation.
            
            field:
                A Rect specifying the 'playing field' boundaries.
                The Creep will bounce off the 'walls' of this 
                field.
                
            init_position:
                A vec2d or a pair specifying the initial position
                of the creep on the screen.
            
            init_direction:
                A vec2d or a pair specifying the initial direction
                of the creep. Must have an angle that is a 
                multiple of 45 degres.
            
            speed: 
                Creep speed, in pixels/millisecond (px/ms)
        """
        Sprite.__init__(self)
        
        self.screen = screen
        self.game = game
        self.speed = speed
        self.field = field
        
        # base_image_0/45 hold the original images, un-rotated
        #
        self.base_image_0 = creep_images[0]
        self.base_image_45 = creep_images[1]
        
        # self.image is the current image representing the creep
        # in the game. It's rotated to the creep's direction.
        #
        self.image = self.base_image_0
        self.explosion_images = explosion_images
        
        # A vector specifying the creep's position on the screen
        #
        self.pos = vec2d(init_position)
        self.prev_pos = vec2d(self.pos)

        # The direction is a normalized vector
        #
        self.direction = vec2d(init_direction).normalized()
        
        self.state = Creep.ALIVE
        self.health = 15
            
    def is_alive(self):
        return self.state in (Creep.ALIVE, Creep.EXPLODING)
    
    def update(self, time_passed):
        """ Update the creep.
        
            time_passed:
                The time passed (in ms) since the previous update.
        """
        if self.state == Creep.ALIVE:
            # Maybe it's time to change the direction ?
            #
            self._compute_direction(time_passed)
            
            # Make the creep image point in the correct direction.
            # Note that two images are used, one for diagonals
            # and one for horizontals/verticals.
            #
            # round() on the angle is necessary, to make it 
            # exact, despite small deviations that may result from
            # floating-point calculations
            #
            if int(round(self.direction.angle)) % 90 == 45:
                self.image = pygame.transform.rotate(
                    self.base_image_45, -(self.direction.angle + 45))
            elif int(round(self.direction.angle)) % 90 == 0:
                self.image = pygame.transform.rotate(
                    self.base_image_0, -self.direction.angle)
            else:
                assert False
            
            # Compute and apply the displacement to the position 
            # vector. The displacement is a vector, having the angle
            # of self.direction (which is normalized to not affect
            # the magnitude of the displacement)
            #
            displacement = vec2d(    
                self.direction.x * self.speed * time_passed,
                self.direction.y * self.speed * time_passed)
            
            self.prev_pos = vec2d(self.pos)
            self.pos += displacement
            
            # When the image is rotated, its size is changed.
            self.image_w, self.image_h = self.image.get_size()
        
        elif self.state == Creep.EXPLODING:
            if self.explode_animation.active:
                self.explode_animation.update(time_passed)
            else:
                self._die()
        
        elif self.state == Creep.DEAD:
            pass
        
    def draw(self):
        """ Blit the creep onto the screen that was provided in
            the constructor.
        """
        if self.state == Creep.ALIVE:
            # The creep image is placed at self.pos. To allow for 
            # smooth movement even when the creep rotates and the 
            # image size changes, its placement is always 
            # centered.
            #
            self.draw_rect = self.image.get_rect().move(
                self.pos.x - self.image_w / 2, 
                self.pos.y - self.image_h / 2)
            self.screen.blit(self.image, self.draw_rect)
            
            # The health bar is 15x4 px.
            #
            health_bar_x = self.pos.x - 7
            health_bar_y = self.pos.y - self.image_h / 2 - 6
            self.screen.fill(   Color('red'), 
                                (health_bar_x, health_bar_y, 15, 4))
            self.screen.fill(   Color('green'), 
                                (   health_bar_x, health_bar_y, 
                                    self.health, 4))
        
        elif self.state == Creep.EXPLODING:
            self.explode_animation.draw()
        
        elif self.state == Creep.DEAD:
            pass
    
    def mouse_click_event(self, pos):
        """ The mouse was clicked in pos.
        """
        if self._point_is_inside(vec2d(pos)):
            self._decrease_health(3)
                
    #------------------ PRIVATE PARTS ------------------#
    
    # States the creep can be in.
    #
    # ALIVE: The creep is roaming around the screen
    # EXPLODING: 
    #   The creep is now exploding, just a moment before dying.
    # DEAD: The creep is dead and inactive
    #
    (ALIVE, EXPLODING, DEAD) = range(3)
    
    def _die(self):
        self.state = Creep.DEAD
        self.kill()
    
    def _compute_direction(self, time_passed):
        """ Finds out where to go
        """
        coord = self.game.xy2coord(self.pos)
        
        if self.game.is_goal_coord(coord):
            self._die()
        else:
            x_mid, y_mid = self.game.coord2xy_mid(coord)
            
            if (    (x_mid - self.pos.x) * (x_mid - self.prev_pos.x) < 0 or
                    (y_mid - self.pos.y) * (y_mid - self.prev_pos.y) < 0):
                next_coord = self.game.next_on_path(coord)
        
                self.direction = vec2d(
                    next_coord[1] - coord[1],
                    next_coord[0] - coord[0]).normalized()
    
    def _point_is_inside(self, point):
        """ Is the point (given as a vec2d) inside our creep's
            body?
        """
        img_point = point - vec2d(  
            int(self.pos.x - self.image_w / 2),
            int(self.pos.y - self.image_h / 2))
        
        try:
            pix = self.image.get_at(img_point)
            return pix[3] > 0
        except IndexError:
            return False
    
    def _decrease_health(self, n):
        """ Decrease my health by n (or to 0, if it's currently
            less than n)
        """
        self.health = max(0, self.health - n)
        if self.health == 0:
            self._explode()

    def _explode(self):
        """ Starts the explosion animation that ends the Creep's
            life.
        """
        self.state = Creep.EXPLODING
        pos = ( self.pos.x - self.explosion_images[0].get_width() / 2,
                self.pos.y - self.explosion_images[0].get_height() / 2)
        self.explode_animation = SimpleAnimation(
            self.screen, pos, self.explosion_images,
            100, 300)


class GridPath(object):
    """ Represents the game grid and answers questions about 
        paths on this grid.
        
        After initialization, call set_blocked for changed 
        information about the state of blocks on the grid, and
        get_next to get the next coordinate on the path to the 
        goal from a given coordinate.
    """
    def __init__(self, nrows, ncols, goal):
        self.map = GridMap(nrows, ncols)
        self.goal = goal
        
        # Path cache. For a coord, keeps the next coord to move
        # to in order to reach the goal. Invalidated when the
        # grid changes (with set_blocked)
        #
        self._path_cache = {}
    
    def get_next(self, coord):
        """ Get the next coordinate to move to from 'coord' 
            towards the goal.
        """
        # If the next path for this coord is not cached, compute
        # it
        #
        if not (coord in self._path_cache):
            self._compute_path(coord)
        
        # _compute_path adds the path for the coord to the cache.
        # If it's still not cached after the computation, it means
        # that no path exists to the goal from this coord.
        #
        if coord in self._path_cache:
            return self._path_cache[coord]
        else:
            return None
    
    def set_blocked(self, coord, blocked=True):
        """ Set the 'blocked' state of a coord
        """
        self.map.set_blocked(coord, blocked)
        
        # Invalidate cache, because the map has changed
        #
        self._path_cache = {}

    def _compute_path(self, coord):
        pf = PathFinder(self.map.successors, self.map.move_cost,
                self.map.move_cost)
        
        # Get the whole path from coord to the goal into a list,
        # and for each coord in the path write the next coord in
        # the path into the path cache
        #
        path_list = list(pf.compute_path(coord, self.goal))
        
        for i, path_coord in enumerate(path_list):
            next_i = i if i == len(path_list) - 1 else i + 1
            self._path_cache[path_coord] = path_list[next_i]


class Game(object):
    # Game parameters
    BG_TILE_IMG = 'images/brick_tile.png'
    SCREEN_WIDTH, SCREEN_HEIGHT = 580, 500
    GRID_SIZE = 20
    FIELD_SIZE = 400, 400
    
    CREEP_FILENAMES = [
        ('images/bluecreep_0.png', 'images/bluecreep_45.png'),
        ('images/greencreep_0.png', 'images/greencreep_45.png'),
        ('images/yellowcreep_0.png', 'images/yellowcreep_45.png'),
        ('images/pinkcreep_0.png', 'images/pinkcreep_45.png'),
    ]
    
    MAX_N_CREEPS = 50

    def __init__(self):
        pygame.init()
        
        self.screen = pygame.display.set_mode(
                        (self.SCREEN_WIDTH, self.SCREEN_HEIGHT), 0, 32)
        self.tile_img = pygame.image.load(self.BG_TILE_IMG).convert_alpha()
        self.tile_img_rect = self.tile_img.get_rect()
        
        self.field_border_width = 4
        field_outer_width = self.FIELD_SIZE[0] + 2 * self.field_border_width
        field_outer_height = self.FIELD_SIZE[1] + 2 * self.field_border_width
        self.field_rect_outer = Rect(20, 60, field_outer_width, field_outer_height)
        self.field_bgcolor = Color(109, 41, 1, 100)
        self.field_border_color = Color(0, 0, 0)
        self.field_box = Box(self.screen, 
            rect=self.field_rect_outer, 
            bgcolor=self.field_bgcolor,
            border_width=self.field_border_width,
            border_color=self.field_border_color)
        
        self.tboard_text = ['The amazing Creeps!']
        self.tboard_rect = Rect(20, 20, field_outer_width, 30)
        self.tboard_bgcolor = Color(50, 20, 0)
        self.tboard = MessageBoard(self.screen,
            rect=self.tboard_rect,
            bgcolor=self.tboard_bgcolor,
            border_width=4,
            border_color=Color('black'),
            text=self.tboard_text,
            font=('tahoma', 18),
            font_color=Color('yellow'))
        
        self.mboard_text = []
        self.mboard_rect = Rect(440, 60, 120, 60)
        self.mboard_bgcolor = Color(50, 20, 0)
        self.mboard = MessageBoard(self.screen,
            rect=self.mboard_rect,
            bgcolor=self.mboard_bgcolor,
            border_width=4,
            border_color=Color('black'),
            text=self.mboard_text,
            font=('verdana', 16),
            font_color=Color('white'))
    
        self.clock = pygame.time.Clock()
        self.paused = False

        self.creep_images= [
            (   pygame.image.load(f1).convert_alpha(),
                pygame.image.load(f2).convert_alpha())
            for (f1, f2) in self.CREEP_FILENAMES]

        explosion_img = pygame.image.load('images/explosion1.png').convert_alpha()
        self.explosion_images = [
            explosion_img, pygame.transform.rotate(explosion_img, 90)]
        
        self.field_rect = self.get_field_rect()
        
        self.entrance_rect = Rect(
            self.field_rect.left,
            self.field_rect.top,
            self.GRID_SIZE * 2,
            self.GRID_SIZE * 2)
        
        self.exit_rect = Rect(
            self.field_rect.right - self.GRID_SIZE * 2,
            self.field_rect.bottom - self.GRID_SIZE * 2,
            self.GRID_SIZE * 2,
            self.GRID_SIZE * 2)
        
        # Create the creeps group and the first creep
        self.creeps = pygame.sprite.Group()
        self.spawn_new_creep()
        
        self.creep_spawn_timer = Timer(500, self.spawn_new_creep)
        
        self.create_walls()
        
        # Create the grid-path representation of the field
        #
        self.grid_nrows = self.FIELD_SIZE[1] / self.GRID_SIZE
        self.grid_ncols = self.FIELD_SIZE[0] / self.GRID_SIZE
        self.goal_coord = (self.grid_nrows - 1, self.grid_ncols - 1)
        self.gridpath = GridPath(
            nrows=self.grid_nrows,
            ncols=self.grid_ncols,
            goal=self.goal_coord)
        
        for wall in self.walls:
            self.gridpath.set_blocked(wall)
    
        self.options = dict(
            draw_grid=False)
    
    def create_walls(self):
        walls_list = []
        
        for r in range(0, 15):
            walls_list.append((r, 6))
            
            if r != 7:
                walls_list.append((r, 3))
                walls_list.append((r, 4))
            
            if r > 4:
                walls_list.append((r, 1))
        
        for r in range(9, 20):
            walls_list.append((r, 10))
        
        for c in range(14, 18):
            walls_list.append((15, c))
        
        self.walls = dict().fromkeys(walls_list, True)
    
    def next_on_path(self, coord):
        """ Given a coord, returns the next coord on the path to
            the goal. None is returned if no path exists from
            the coord.
        """
        return self.gridpath.get_next(coord)
    
    def xy2coord(self, pos):
        """ Convert a (x, y) pair to a (nrow, ncol) coordinate
        """
        x, y = (pos[0] - self.field_rect.left, pos[1] - self.field_rect.top)
        return (int(y) / self.GRID_SIZE, int(x) / self.GRID_SIZE)
    
    def coord2xy_mid(self, coord):
        """ Convert a (nrow, ncol) coordinate to a (x, y) pair,
            where x,y is the middle of the square at the coord
        """
        nrow, ncol = coord
        return (
            self.field_rect.left + ncol * self.GRID_SIZE + self.GRID_SIZE / 2, 
            self.field_rect.top + nrow * self.GRID_SIZE + self.GRID_SIZE / 2)
    
    def is_goal_coord(self, coord):
        return coord == self.goal_coord
    
    _spawned_creep_count = 0
    def spawn_new_creep(self):
        if self._spawned_creep_count >= self.MAX_N_CREEPS:
            return
        
        self.creeps.add(
            Creep(  screen=self.screen,
                    game=self,
                    creep_images=choice(self.creep_images),
                    explosion_images=self.explosion_images,
                    field=self.field_rect,
                    init_position=( self.field_rect.left + self.GRID_SIZE / 2,
                                    self.field_rect.top + self.GRID_SIZE / 2),
                    init_direction=(1, 1),
                    speed=0.05))
        self._spawned_creep_count += 1
    
    def get_field_rect(self):
        """ Return the internal field rect - the rect of the game
            field exluding its border.
        """
        return self.field_box.get_internal_rect()
    
    def draw_background(self):
        img_rect = self.tile_img.get_rect()
        nrows = int(self.screen.get_height() / img_rect.height) + 1
        ncols = int(self.screen.get_width() / img_rect.width) + 1
        
        for y in range(nrows):
            for x in range(ncols):
                img_rect.topleft = (x * img_rect.width, 
                                    y * img_rect.height)
                self.screen.blit(self.tile_img, img_rect)
    
    def draw_portals(self):
        entrance_sf = pygame.Surface((self.entrance_rect.w, self.entrance_rect.h))
        entrance_sf.fill(Color(80, 200, 80))
        entrance_sf.set_alpha(150)
        self.screen.blit(entrance_sf, self.entrance_rect)
        
        exit_sf = pygame.Surface((self.exit_rect.w, self.exit_rect.h))
        exit_sf.fill(Color(200, 80, 80))
        exit_sf.set_alpha(150)
        self.screen.blit(exit_sf, self.exit_rect) 
    
    def draw_grid(self):
        for y in range(self.grid_nrows + 1):
            pygame.draw.line(
                self.screen,
                Color(50, 50, 50),
                (self.field_rect.left, self.field_rect.top + y * self.GRID_SIZE - 1),
                (self.field_rect.right - 1, self.field_rect.top + y * self.GRID_SIZE - 1))
        
        for x in range(self.grid_ncols + 1):
            pygame.draw.line(
                self.screen,
                Color(50, 50, 50),
                (self.field_rect.left + x * self.GRID_SIZE - 1, self.field_rect.top),
                (self.field_rect.left + x * self.GRID_SIZE - 1, self.field_rect.bottom - 1))
    
    def draw_walls(self):
        wallcolor = Color(140, 140, 140)
        
        for wall in self.walls:
            nrow, ncol = wall
            
            pos_x = self.field_rect.left + ncol * self.GRID_SIZE + self.GRID_SIZE / 2
            pos_y = self.field_rect.top + nrow * self.GRID_SIZE + self.GRID_SIZE / 2
            radius = 3
            
            pygame.draw.polygon(self.screen, wallcolor,
                [   (pos_x - radius, pos_y), (pos_x, pos_y + radius),
                    (pos_x + radius, pos_y), (pos_x, pos_y - radius)])
            
            if (nrow + 1, ncol) in self.walls:
                pygame.draw.line(self.screen, wallcolor,
                    (pos_x, pos_y), (pos_x, pos_y + self.GRID_SIZE), 3)
            if (nrow, ncol + 1) in self.walls:
                pygame.draw.line(self.screen, wallcolor,
                    (pos_x, pos_y), (pos_x + self.GRID_SIZE, pos_y), 3)
    
    def draw(self):
        self.draw_background()
        self.field_box.draw()
        
        if self.options['draw_grid']:
            self.draw_grid()
        
        self.draw_walls()
        
        self.tboard.draw()
        self.mboard.text = self.mboard_text
        self.mboard.draw()
        
        for creep in self.creeps:
            creep.draw()
        
        self.draw_portals()
        
    def run(self):
        # The main game loop
        #
        while True:
            # Limit frame speed to 30 FPS
            #
            time_passed = self.clock.tick(30)
            #~ time_passed = self.clock.tick()
            #~ print time_passed
            
            # If too long has passed between two frames, don't
            # update (the game must have been suspended for some
            # reason, and we don't want it to "jump forward"
            # suddenly)
            #
            if time_passed > 100:
                continue
            
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.quit()
                elif event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_SPACE:
                        self.paused = not self.paused
                    elif event.key == pygame.K_g:
                        if pygame.key.get_mods() & pygame.KMOD_CTRL:
                            self.options['draw_grid'] = not self.options['draw_grid']
                elif (  event.type == pygame.MOUSEBUTTONDOWN and
                        event.button == 1):
                    for creep in self.creeps:
                        creep.mouse_click_event(event.pos)
            
            if not self.paused:
                msg1 = 'Creeps: %d' % len(self.creeps)
                msg2 = ''

                self.mboard_text = [msg1, msg2]
                
                self.creep_spawn_timer.update(time_passed)
                
                # Update and all creeps
                for creep in self.creeps:
                    creep.update(time_passed)
                    
                self.draw()
                
            pygame.display.flip()

    def quit(self):
        sys.exit()


if __name__ == "__main__":
    game = Game()
    game.run()
