from collections import defaultdict


class DGraph(object):
    """Directed graph.

    Represents edges between arbitrary objects serving as nodes. The only
    requirement for nodes is that they're hashable.
    """
    def __init__(self):
        self.edges = defaultdict(set)

    def add_edges(self, *pairs):
        """Add edges to the graph.

        Accepts an arbitrary number of pairs (u, v), meaning that there's an
        edge in the graph from u to v.
        """
        for u, v in pairs:
            self.edges[u].add(v)
            if v not in self.edges:
                self.edges[v] = set()

    def successors(self, v):
        """Iterator over all the successors of node v in the graph.

        The successors of node v are all the nodes that v has an edge to.
        """
        return iter(self.edges[v])

    def nodes(self):
        """Iterator over all the nodes in the graph."""
        return iter(self.edges)

    def render(self, filename='graph.png'):
        """Render the graph to a PNG file using pygraphviz.

        filename: name of the output file.

        To use this method, pygraphviz has to be installed.
        """
        try:
            import pygraphviz as pgv
            agv = pgv.AGraph(directed=True, strict=False)
            for u in self.edges:
                for v in self.edges[u]:
                    agv.add_edge(u, v)
            agv.layout('dot')
            agv.draw(filename)
            print('Rendered graph to "{0}"'.format(filename))
        except ImportError as e:
            print('Unable to import pygraphviz - not rendering')
            print(e)
        

def dfs(graph, root, visitor):
    """DFS over a graph.

    Start with node 'root', calling 'visitor' for every visited node.
    """
    visited = set()
    def dfs_walk(node):
        visited.add(node)
        visitor(node)
        for succ in graph.successors(node):
            if not succ in visited:
                dfs_walk(succ)
    dfs_walk(root)


def postorder(graph, root):
    """Return a post-order ordering of nodes in the graph."""
    visited = set()
    order = []
    def dfs_walk(node):
        visited.add(node)
        for succ in graph.successors(node):
            if not succ in visited:
                dfs_walk(succ)
        order.append(node)
    dfs_walk(root)
    return order


def postorder_unrooted(graph):
    """Unrooted post-order traversal of a graph.
    
    Restarts traversal as long as there are undiscovered nodes. Returns a list
    of lists, each of which is a post-order ordering of nodes discovered while
    restarting the traversal.
    """
    allnodes = set(graph.nodes())
    visited = set()
    orders = []
    def dfs_walk(node):
        visited.add(node)
        for succ in graph.successors(node):
            if not succ in visited:
                dfs_walk(succ)
        orders[-1].append(node)
    while len(allnodes) > len(visited):
        # While there are still unvisited nodes in the graph, pick one at random
        # and restart the traversal from it.
        remaining = allnodes - visited
        root = remaining.pop()
        orders.append([])
        dfs_walk(root)
    return orders


def postorder_3color(graph, root):
    """Return a post-order ordering of nodes in the graph.
    
    Prints CYCLE notifications when graph cycles ("back edges") are discovered.
    """
    color = dict()
    order = []
    def dfs_walk(node):
        color[node] = 'grey'
        for succ in graph.successors(node):
            if color.get(succ) == 'grey':
                print 'CYCLE: {0}-->{1}'.format(node, succ)
            if succ not in color:
                dfs_walk(succ)
        order.append(node)
        color[node] = 'black'
    dfs_walk(root)
    return order


if __name__ == '__main__':
    gg = DGraph()
    gg.add_edges(('x', 't'), ('x', 'b'), ('x', 'c'))
    gg.add_edges(('c', 'e'), ('e', 'm'), ('m', 'c'))
    gg.add_edges(('b', 'd'), ('e', 'd'), ('t', 'b'))
    gg.add_edges(('d', 'g'), ('g', 'd'))
    gg.render()

    print('all nodes')
    print(list(gg.nodes()))

    print('post', postorder(gg, 'x'))
    print('post_3color', postorder_3color(gg, 'x'))
