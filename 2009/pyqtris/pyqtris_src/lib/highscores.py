import base64
import pickle


class HighScores(object):
    """ Keeps track of a high-scores list, allowing to save
        and load it from a file, add new scores, and return 
        a sorted list.
    """
    def __init__(self, max_length=10):
        # The scorelist is a list of 2-tuples (name, score)
        #
        self.max_length = max_length
        self.scorelist = []
    
    def lowest_score(self):
        """ The lowest score currently in the list.
            If the list is still incomplete, returns 0
        """
        return 0 if len(self.scorelist) < self.max_length else self.scorelist[-1][1]
    
    def add_score(self, name, score):
        """ Adds a score to the high-scores. The score is only
            added if it's strictly higher than the lowest score
            currently on the board, or if the current board
            is shorter than the maximal length.
        """
        self.scorelist.append((name, score))
        self.scorelist.sort(reverse = True, cmp = lambda a, b: cmp(a[1], b[1]))
        
        if len(self.scorelist) > self.max_length:
            self.scorelist = self.scorelist[0:-1]
 
    def get_list(self):
        """ Returns the high-scores list.
            The list contains (name, score) tuples and is sorted
            in a descending order (highest score first).
        """
        return self.scorelist
 
    def printme(self):
        for i in self.scorelist:
            print i
 
    def save_to_file(self, filename):
        """ Saves the high-scores list to a file.
        """
        file = open(filename, 'w')
        str = base64.encodestring(pickle.dumps(self.scorelist))
        file.write(str)
        file.close()

    def load_from_file(self, filename):
        """ Loads the high-scores list from a file.
        """
        file = open(filename, 'r')
        str = base64.decodestring(file.read())
        self.scorelist = pickle.loads(str)
        file.close()


if __name__ == "__main__":
    FILENAME = 'pyqtris_highscores'
    ds = HighScores(10) 
    
    #~ ds.load_from_file(FILENAME)
    #~ ds.printme()
        
    ds.add_score('joe', 15)
    ds.add_score('qjoe', 15)
    ds.add_score('nexus', 155)
    ds.add_score('ntxus', 19)
    ds.add_score('p2', 199)
    ds.add_score('p3', 3319)
    ds.add_score('losersRUS', 219)
    ds.add_score('jesus', 6775)
    ds.add_score('morti', 800)

    ds.save_to_file(FILENAME)
