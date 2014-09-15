import heapq


class PriorityQueueSet(object):
    """ Combined priority queue and set data structure. Acts like
        a priority queue, except that its items are guaranteed to
        be unique.
        
        Provides O(1) membership test and O(log N) removal of the 
        *smallest* item. Addition is more complex. When the item 
        doesn't exist, it's added in O(log N). When it already 
        exists, its priority is checked against the new item's
        priority in O(1). If the new item's priority is smaller,
        it is updated in the queue. This takes O(N).
        
        Important: The items you store in the queue have identity
        (that determines when two items are the same, as far as
        you're concerned) and priority. Therefore, you must 
        implement the following operators for them: __hash__, 
        __cmp__ and __eq__.
        
        *   __eq__ will be used for exact comparison of items. It 
            must return True if and only if the items are identical
            from your point of view (although their priorities can
            be different)
        *   __cmp__ will be used to compare priorities. Two items
            can be different and have the same priority, and even
            be equal but have different priorities (though they
            can't be in the queue at the same time)
        *   __hash__ will be used to hash the items for 
            efficiency. To implement it, you almost always have 
            to just call hash() on the attribute you're comparing
            in __eq__
            
        Note that for native Python objects (strings, tuples, 
        etc.) these operators are already defined as needed.
    """
    def __init__(self):
        """ Create a new PriorityQueueSet
        """
        self.set = {}
        self.heap = []    

    def __len__(self):
        return len(self.heap)

    def has_item(self, item):
        """ Check if *item* exists in the queue
        """
        return item in self.set
    
    def pop_smallest(self):
        """ Remove and return the smallest item from the queue.
            IndexError will be thrown if the queue is empty.
        """
        smallest = heapq.heappop(self.heap)
        del self.set[smallest]
        return smallest
    
    def add(self, item):
        """ Add *item* to the queue. 
        
            If such an item already exists, its priority will be 
            checked versus *item*. If *item*'s priority is better
            (i.e. lower), the priority of the existing item in the 
            queue will be updated.
        
            Returns True iff the item was added or updated.
        """
        if not item in self.set:
            self.set[item] = item
            heapq.heappush(self.heap, item)
            return True
        elif item < self.set[item]:
            # No choice but to search linearly in the heap
            #
            for idx, old_item in enumerate(self.heap):
                if old_item == item:
                    del self.heap[idx]
                    self.heap.append(item)
                    heapq.heapify(self.heap)
                    self.set[item] = item
                    return True
        
        return False
        

if __name__ == "__main__":
    import unittest
    
    #-------------------------------------------------------------
    class TestPriorityQueueSet(unittest.TestCase):
        def test_ints(self):
            # Since the int's priority is always its value, here
            # we won't test insertion of existing items with lower
            # priorities
            #
            pqs = PriorityQueueSet()
            for k in [3, 5, 2, 2, 99, 23]:
                pqs.add(k)
            
            self.assert_(pqs.has_item(3))
            self.assert_(pqs.has_item(2))
            self.assert_(pqs.has_item(99))
            self.assert_(not pqs.has_item(4))
            self.assertEqual(len(pqs), 5)
            
            self.assertEqual(pqs.pop_smallest(), 2)
            self.assertEqual(pqs.pop_smallest(), 3)
            self.assertEqual(len(pqs), 3)
    
            pqs.add(1)
            self.assertEqual(len(pqs), 4)
            self.assertEqual(pqs.pop_smallest(), 1)
            self.assertEqual(pqs.pop_smallest(), 5)
            self.assertEqual(pqs.pop_smallest(), 23)
            self.assertEqual(pqs.pop_smallest(), 99)
            self.assertRaises(IndexError, pqs.pop_smallest)
            self.assertEqual(len(pqs), 0)
            
            self.assert_(pqs.add(6))
            self.assert_(pqs.add(16))
            self.assert_(pqs.add(2))
            self.assertEqual(pqs.pop_smallest(), 2)
            self.assert_(not pqs.add(6))
            self.assert_(not pqs.add(16))
            self.assert_(pqs.add(2))
            self.assertEqual(pqs.pop_smallest(), 2)
            self.assertEqual(pqs.pop_smallest(), 6)
            self.assertEqual(pqs.pop_smallest(), 16)
            self.assertRaises(IndexError, pqs.pop_smallest)

        def test_hashable(self):
            class Node(object):
                def __init__(self, value, cost):
                    self.value = value
                    self.cost = cost
                
                def __eq__(self, other):
                    return self.value == other.value
                
                def __cmp__(self, other):
                    return cmp(self.cost, other.cost)
                
                def __hash__(self):
                    return hash(self.value)
                    
                def __repr__(self):
                    return "^%s&%s^" % (self.value, self.cost)
        
            pqs = PriorityQueueSet()
            pqs.add(Node('five', 5))
            pqs.add(Node('one', 1))
            pqs.add(Node('eight', 8))
            self.assertEqual(len(pqs), 3)
            
            self.assert_(not pqs.add(Node('five', 55)))
            self.assertEqual(len(pqs), 3)

            self.assert_(not pqs.has_item(Node('three', 3)))
            self.assert_(not pqs.has_item(Node('thirteen', 13)))
            self.assert_(pqs.has_item(Node('one', 1)))
            self.assert_(pqs.has_item(Node('five', 5)))
            
            self.assertEqual(pqs.pop_smallest(), Node('one', 1))
            self.assertEqual(pqs.pop_smallest(), Node('five', 5))
            self.assertEqual(len(pqs), 1)
            
            self.assertEqual(pqs.pop_smallest(), Node('eight', 8))
            self.assertRaises(IndexError, pqs.pop_smallest)
            self.assertEqual(len(pqs), 0)
            
            # Now test with replacement of value by lower
            # priority
            #
            pqs.add(Node('five', 5))
            pqs.add(Node('one', 1))
            pqs.add(Node('eight', 8))
            self.assert_(pqs.add(Node('five', 0)))
            
            self.assert_(pqs.has_item(Node('five', 5)))
            self.assertEqual(len(pqs), 3)
            
            self.assertEqual(pqs.pop_smallest(), Node('five', 0))
            self.assertEqual(pqs.pop_smallest(), Node('one', 1))
            self.assertEqual(pqs.pop_smallest(), Node('eight', 8))
            self.assertRaises(IndexError, pqs.pop_smallest)
            self.assertEqual(len(pqs), 0)
            
            # More replacements
            # 
            pqs.add(Node('five', 5))
            pqs.add(Node('one', 1))
            pqs.add(Node('eight', 8))
            pqs.add(Node('eight', 3))
            pqs.add(Node('five', 4))
            pqs.add(Node('five', 24))
            pqs.add(Node('one', 33))
            
            self.assertEqual(pqs.pop_smallest(), Node('one', 1))
            self.assertEqual(pqs.pop_smallest(), Node('eight', 3))
            self.assertEqual(pqs.pop_smallest(), Node('five', 4))
            self.assertRaises(IndexError, pqs.pop_smallest)
            self.assertEqual(len(pqs), 0)
    #-------------------------------------------------------------
    
    unittest.main()
