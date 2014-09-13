class LiveDataFeed(object):
    """ A simple "live data feed" abstraction that allows a reader 
        to read the most recent data and find out whether it was 
        updated since the last read. 
        
        Interface to data writer:
        
        add_data(data):
            Add new data to the feed.
        
        Interface to reader:
        
        read_data():
            Returns the most recent data.
            
        has_new_data:
            A boolean attribute telling the reader whether the
            data was updated since the last read.    
    """
    def __init__(self):
        self.cur_data = None
        self.has_new_data = False
    
    def add_data(self, data):
        self.cur_data = data
        self.has_new_data = True
    
    def read_data(self):
        self.has_new_data = False
        return self.cur_data


if __name__ == "__main__":
    pass

