from __future__ import print_function
import random, time


class Timer(object):
    def __init__(self, name=None):
        self.name = name
    
    def __enter__(self):
        self.tstart = time.time()
        
    def __exit__(self, type, value, traceback):
        if self.name:
            print('[%s]' % self.name, end=' ')
        print('Elapsed: %s' % (time.time() - self.tstart))


def get_all_from_queue(Q):
    """ Generator to yield one after the others all items 
        currently in the queue Q, without any waiting.
    """
    try:
        while True:
            yield Q.get_nowait( )
    except Queue.Empty:
        raise StopIteration


def get_item_from_queue(Q, timeout=0.01):
    """ Attempts to retrieve an item from the queue Q. If Q is
        empty, None is returned.
        
        Blocks for 'timeout' seconds in case the queue is empty,
        so don't use this method for speedy retrieval of multiple
        items (use get_all_from_queue for that).
    """
    try: 
        item = Q.get(True, timeout)
    except Queue.Empty: 
        return None
    
    return item


def flatten(iterables):
    """ Flatten an iterable of iterables. Returns a generator.
        
        list(flatten([[2, 3], [5, 6]])) => [2, 3, 5, 6]
    """
    return (elem for iterable in iterables for elem in iterable)


def argmin_list(seq, func):
    """ Return a list of elements of seq[i] with the lowest 
        func(seq[i]) scores.
        >>> argmin_list(['one', 'to', 'three', 'or'], len)
        ['to', 'or']
    """
    best_score, best = func(seq[0]), []
    for x in seq:
        x_score = func(x)
        if x_score < best_score:
            best, best_score = [x], x_score
        elif x_score == best_score:
            best.append(x)
    return best


def argmin_random_tie(seq, func):
    """ Return an element with lowest func(seq[i]) score; break 
        ties at random.
    """
    return random.choice(argmin_list(seq, func))


def argmin(seq, func):
    """ Return an element with lowest func(seq[i]) score; tie goes 
        to first one.
        >>> argmin(['one', 'to', 'three'], len)
        'to'
    """
    return min(seq, key=func)


def argmax_list(seq, func):
    """ Return a list of elements of seq[i] with the highest 
        func(seq[i]) scores.
        >>> argmax_list(['one', 'three', 'seven'], len)
        ['three', 'seven']
    """
    return argmin_list(seq, lambda x: -func(x))


def argmax_random_tie(seq, func):
    """ Return an element with highest func(seq[i]) score; break 
        ties at random.
    """
    return random.choice(argmax_list(seq, func))


def argmax(seq, func):
    """ Return an element with highest func(seq[i]) score; tie 
        goes to first one.
        >>> argmax(['one', 'to', 'three'], len)
        'three'
    """
    return max(seq, key=func)


#-----------------------------------------------------------------
if __name__ == "__main__":
    #~ print list(flatten([[1, 2], (4, 5), [5], [6, 6, 8]]))
    #~ print argmin_random_tie(['one', 'to', 'three', 'or'], len)
    
    print(min(['one', 'to', 'three', 'or'], key=len))
