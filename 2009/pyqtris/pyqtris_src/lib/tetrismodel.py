import copy
import pprint
import random


class TetrisBoard(object):
    """ Represents a Tetris board.
        
        The board is where the actual tetris game takes place.
        It is a 2D array of cells, some of which are already
        occupied. It also keeps track of the current figure,
        allowing to move and rotate it, and check for collisions.
        
        The board is taking care of all the non-graphical action
        in the tetris game, and hence can be used separately of
        the graphical environment.
        
        To borrow a term from the MVC pattern, this board is the
        model of a tetris game.
    """
    def __init__(self, nrows, ncols):
        self.nrows = nrows
        self.ncols = ncols
        self.board = []
        self.fig = None
        self.figcenter = None
        
        for i in range(nrows):
            self.board.append([0] * ncols)
    
    def reset_board(self, board):
        """ Resets the internal board to the given one. This 
            destroys the current internal board and invalidates 
            the active figure.
        """
        self.board = board
        self.fig = None
        self.figcenter = None   
    
    def spawn_figure(self, figure):
        """ Creates a new figure on the board.
            
            Returns False if there's no space for the new figure
            (this basically means game over), and True if 
            everything is OK and the figure was created.
        """
        # We have to calculate where to place the new figure
        # on the board (that is, where to place its central
        # block).
        #
        # First of all try to place the new figure in the 
        # horizontal center of the board
        #
        figwidth = figure.width()
        figcol = (self.ncols - figwidth) / 2 + (-figure.mincol)

        # Now, find out the vertical position so that the whole
        # figure will have space on the board
        #
        figrow = -figure.minrow
        
        # Is is possible to fit the figure in these coordinates?
        #
        figcenter = figrow, figcol
        
        if self._figure_conflicts(figure, figcenter):
            return False
        
        # Congrats, we have a new figure!
        #
        self.fig = figure
        self.figcenter = figcenter
        return True

    #
    # The following functions implement movement and rotation
    # of the current figure on the board. They all return True
    # if the movement/rotation were performed successfully, and
    # False if the operation is invalid (creates a conflict).
    #
    def move_figure_right(self):
        return self._move_figure((0, 1))
        
    def move_figure_left(self):
        return self._move_figure((0, -1))
    
    def move_figure_down(self):
        return self._move_figure((1, 0))
    
    def rotate_figure(self):
        newfig = self.fig.copy()
        newfig.rotate()
        
        if self._figure_conflicts(newfig, self.figcenter):
            return False
        
        self.fig = newfig 
        return True
    
    def figure_can_move_down(self):
        """ Returns False if the current figure can no longer be
            moved down, and True if it still can.
        """
        newrow, newcol = self.figcenter[0] + 1, self.figcenter[1]
        return not self._figure_conflicts(self.fig, (newrow, newcol))
    
    def finish_fall(self):
        """ Finishes the figure's fall. This can result in one
            or more "completed" rows.
            
            Returns the numbers of rows that were completed in 
            a list, and an empty list if no rows were completed.
            Note: rows are counted from the top down, with the
            uppermost row being number 0. The completed rows 
            are removed from the board, and the rows above them
            are shifted down.
            
            After this method is called, the current figure on
            the board is no longer valid, and a new one must
            be spawned.
        """
        # Turn the active figure into a bunch of inactive blocks
        #
        self.board = self.board_with_active_figure()
        
        # Invalidate the active figure, its role is finished
        #
        self.fig = None
        self.figcenter = None
        
        # Now, run over the rows of the board from the top down.
        # Find completed rows, remove them, and shift the rows
        # above them down
        #
        completed_rows = []
        
        for nrow in range(self.nrows):
            if self._row_is_completed(nrow):
                completed_rows.append(nrow)
        
        for completed_row in completed_rows:
            self._remove_completed_row(completed_row)
        
        return completed_rows
    
    def print_in_ascii(self):
        board = copy.deepcopy(self.board)
        
        # This is a bit of a hack worth explaining. The board is 
        # a 2D array in which there's either 0 for an empty space,
        # or a color object provided from outside.
        # In the ascii drawing we don't care about the specific 
        # color but want to distinct the current figure from all
        # the stationary blocks. For this purpose, we place 1 
        # and 2 (for the center) in the board in place of the 
        # current figure's blocks. These values definitely won't
        # be equal to the color object of the stationary blocks.
        #
        if self.fig:
            for row, col in self.fig.coords: 
                arow = self.figcenter[0] + row
                acol = self.figcenter[1] + col

                board[arow][acol] = 1
                board[self.figcenter[0]][self.figcenter[1]] = 2

        for row in range(self.nrows):
            for col in range(self.ncols):
                c = board[row][col]
                
                if c == 0: print ".",
                elif c == 1: print "o",
                elif c == 2: print "O",
                else: print "#",
                    
            print ""

    def board_with_active_figure(self):
        """ Creates a board where the active figure is embedded 
            as blocks, and returns it.
        """
        board = copy.deepcopy(self.board)
        
        if self.fig:
            for row, col in self.fig.coords: 
                arow = self.figcenter[0] + row
                acol = self.figcenter[1] + col
                assert board[arow][acol] == 0
                board[arow][acol] = self.fig.color
        
        return board

    def _move_figure(self, offset):
        """ Moves the current figure by the given offset. The
            offset is a tuple (row, col).
            
            Returns True if the figure was successfully moved,
            and False if the move creates a conflict.
        """
        if not self.figcenter: return False
            
        newrow = self.figcenter[0] + offset[0]
        newcol = self.figcenter[1] + offset[1]
        
        if self._figure_conflicts(self.fig, (newrow, newcol)):
            return False
        
        self.figcenter = newrow, newcol
        return True

    def _figure_conflicts(self, fig, figcenter):
        """ Does the figure conflict with existing blocks
            on the board, or the board's edges?
        """
        for row, col in fig.coords:
            arow, acol = figcenter[0] + row, figcenter[1] + col
            
            if not 0 <= acol < self.ncols: return True
            if not 0 <= arow < self.nrows: return True
            if self.board[arow][acol] != 0: return True
        
        return False
    
    def _row_is_completed(self, nrow):
        """ Is this row on the board "completed", i.e. full of
            blocks ?
        """
        for ncol in range(self.ncols):
            if self.board[nrow][ncol] == 0: return False
        
        return True
        
    def _remove_completed_row(self, nrow):
        """ Removes the given row from the board, shifting all the
            rows above it down by one.
            
            Note: assumes that nrow is inclusively inside the 
            range (1, self.nrows - 1)
        """
        assert 1 <= nrow < self.nrows
        
        del self.board[nrow]
        self.board.insert(0, [0] * self.ncols)


class FigureBank(object):
    """ A "bank" of tetris figures. 
        Basically this is a glorified abstraction of an array
        with a couple of mnemonic operations.
    """
    def __init__(self, bank = []):
        self.bank = bank
    
    def add_figure(self, figure):
        self.bank.append(figure)
    
    def clear(self):
        self.bank = []
    
    def get_random(self):
        return random.choice(self.bank)


class Figure(object):
    """ Represents a Tetris figure.
        
        Each figure has an implicit center at 0,0 and a list of
        coordinates (2-tuples) which represent the blocks relative
        to the center. 
        
        For example, suppose the list of coords is:
            [(0,0), (-1,0), (0,-1), (1,-1)]
        
        Then the figure is (o - block, O - central block):
        
             oo
            oO
        
        Accessible attributes:
            color, coords, maxcol, maxrow, mincol, minrow
    """
    def __init__(self, coords = [], rotatable = True, color = None):
        self.coords = coords
        self.rotatable = rotatable
        self.color = color
        self._compute_min_max_offsets()

    def copy(self):
        return copy.deepcopy(self)
        
    def width(self): return (self.maxcol - self.mincol) + 1
    def height(self): return (self.maxrow - self.minrow) + 1

    def rotate(self, clockwise = True):
        """ Rotates the figure's coordinates
        """
        if not self.rotatable:
            return
        
        new_coords = []
        
        for (row, col) in self.coords:
            if clockwise:
                row, col = col, -row
            else:
                row, col = -col, row
            new_coords.append((row, col))
            self.coords = new_coords

    def print_in_ascii(self):
        """ The figure will display itself by printing its
            blocks to stdout.
            
            o - non-center block
            O - center-block
            . - blank
            * - center in which there's no block
            
            (only works well with fixed font, of course)
        """
        # Build 9x9 field, assuming it's enough for all figures.
        # The figure's center is at the middle.
        #
        field = []
        
        for col in range(9): field.append([0] * 9)
        
        for (col, row) in self.coords:
            field[4 + col][4 + row] = 1
        
        for col in range(9):
            for row in range(9):
                if field[col][row] == 1:
                    char = 'O' if (row, col) == (4, 4) else 'o'
                else:
                    char = '*' if (row, col) == (4, 4) else '.'
                print char,
            print ""

    def _compute_min_max_offsets(self):
        self.maxrow = self.maxcol = -99
        self.minrow = self.mincol = 99
        
        for (row, col) in self.coords:
            if row > self.maxrow: self.maxrow = row
            if row < self.minrow: self.minrow = row
            if col > self.maxcol: self.maxcol = col
            if col < self.mincol: self.mincol = col


if __name__ == "__main__":  
    #~ x = T
    #~ x.PrintInAscii()
    #~ print ""
    #~ x.Rotate()
    #~ x.PrintInAscii()
    
    nrows = 12
    ncols = 10
    tb = TetrisBoard(nrows, ncols)
    
    myboard = [
            [0] * ncols,
            [0] * ncols,
            [0] * ncols,
            [0] * ncols,                    # 3
            [0] * ncols,
            [0] * ncols,
            [0] * ncols,
            [0] * ncols,                    # 7
            [6, 6, 6, 6, 6, 6, 6, 6, 6, 6],
            [6, 0, 6, 6, 6, 6, 6, 6, 6, 6],
            [6, 0, 6, 6, 6, 6, 0, 6, 6, 6],
            [6, 6, 6, 6, 6, 6, 6, 6, 6, 6], # 11
        ]
    
    tb.reset_board(myboard)
    tb.spawn_figure(Figure([(0,0), (-1,0), (0,-1), (1,-1)]))

    tb.print_in_ascii()
    
    completed_rows = []

    for nrow in range(nrows):
        if tb._row_is_completed(nrow):
            completed_rows.append(nrow)

    print tb.finish_fall()
    tb.print_in_ascii()

    #~ print completed_rows
    #~ tb._TetrisBoard__RemoveCompletedRow(11)
    
    #~ tb.PrintInAscii()
    
    import sys
    sys.exit(6)
    
    if not tb.spawn_figure(I): raise Exception, "RAAR"
    tb.move_figure_right()
    for i in range(8): tb.move_figure_down()
        
    tb.rotate_figure()
    tb.move_figure_down()
    tb.finish_fall()

    tb.print_in_ascii()

    
