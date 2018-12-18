
import sys

import numpy as np
import networkx as nx


class WorldObject:
    """
    A base class for any object in a world

    Attributes
    ----------
    x: int
        the x-coordinate
    y: int
        the y-coordinate
    world: World
        the owner of this object
    """
    def __init__(self, x, y, world):
        self.x = x
        self.y = y
        self.world = world

    def __lt__(self, other):
        """ Sort based on reading order of x,y coordinates """
        if self.y > other.y:
            return False
        elif self.y < other.y:
            return True
        else:
            return self.x < other.x


class Wall(WorldObject):
    """ A class for wall """
    pass


class Space(WorldObject):
    """ A class for an open space """
    pass


class MoveableObject(WorldObject):
    """
    Base class for an object that can move an participate in combat

    Attributes
    ----------
    attack_power: int
        the attacking power
    hit_points: int
        the hit points
    """
    target = None

    def __init__(self, x, y, world):
        super(MoveableObject, self).__init__(x, y, world)
        self.attack_power = 3
        self.hit_points = 200

    def move(self):
        """
        Find where the object can move and then call the world
        to do the actual moving.

        If object after move is in range of a target, it will attack
        """
        candidates = []
        path_lens = {}
        next_nodes = {}
        min_path_len = 1E8
        for target in self.world.goblins_and_elves:
            if isinstance(target, self.__class__):
                continue
            for neigh in self.world.graph.neighbors(target):
                if not isinstance(neigh, Space):
                    continue
                try:
                    next_node, path_len = self.world.route(self, neigh, min_path_len)
                except:
                    pass
                else:
                    if min_path_len > path_len:
                        min_path_len = path_len
                    candidates.append(neigh)
                    path_lens[neigh] = path_len
                    next_nodes[neigh] = next_node

        if len(candidates) == 0:
            return

        candidates = sorted([c for c in candidates if path_lens[c] == min_path_len])
        self.world.swap(self, next_nodes[candidates[0]])
        self.attack()

    def attack(self):
        """
        Check for any suitable target and if one is found wound it
        """
        hp = {}
        targets = []
        for neigh in self.world.graph.neighbors(self):
            if not isinstance(neigh, Wall) and not isinstance(neigh, Space) and not isinstance(neigh, self.__class__):
                hp[neigh] = neigh.hit_points
                targets.append(neigh)

        if len(targets) == 0:
            return
        min_pts = min(p for _, p in hp.items())
        targets = sorted([t for t in targets if hp[t] == min_pts])
        targets[0].wound(self.attack_power)

    def wound(self, power):
        """
        Called when wounded by an attack
        """
        self.hit_points -= power
        if self.hit_points <= 0:
            self.world.remove(self)


class Goblin(MoveableObject):
    """ A class for a goblin """
    pass


class Elf(MoveableObject):
    """ A class for an elf """

    def __init__(self, x, y, world, power=3):
        super(Elf, self).__init__(x, y, world)
        self.attack_power = power


class ElfLossException(Exception):
    pass


class World:
    """
    A class encapsulating the world

    Attributes
    ----------
    walls: dict
        the Wall objects. keys are (int, int) tuples of the object coordinates
    elves: dict
        the Elf objects. keys are (int, int) tuples of the object coordinates
    goblins: dict
        the Goblin objects. keys are (int, int) tuples of the object coordinates
    goblins_and_elves : list
        a list of all Goblin and Elf objects
    graph: networkx.Graph
        a graph representation of the world
    grid: numpy.ndarray
        a matrix representation of the world
    spaces: dict
        the Elf objects. keys are (int, int) tuples of the object coordinates
    """
    def __init__(self, filename, elves_power=3):

        self.grid = []
        self.goblins = {}
        self.elves = {}
        self.walls = {}
        self.spaces = {}
        self.graph = nx.Graph()

        with open(filename, 'r') as f:
            for line in f.readlines():
                self.grid.append(list(line.strip()))
        self.grid = np.asarray(self.grid).T

        for x in range(self.grid.shape[0]):
            for y in range(self.grid.shape[1]):
                if self.grid[x, y] == 'G':
                    self.goblins[(x, y)] = Goblin(x, y, self)
                elif self.grid[x, y] == 'E':
                    self.elves[(x, y)] = Elf(x, y, self, elves_power)
                elif self.grid[x, y] == '#':
                    self.walls[(x, y)] = Wall(x, y, self)
                elif self.grid[x, y] == '.':
                    self.spaces[(x, y)] = Space(x, y, self)
                self.graph.add_node(self._object(x, y))

        self._add_edges()
        self.goblins_and_elves = [g for _, g in self.goblins.items()]
        self.goblins_and_elves.extend([e for _, e in self.elves.items()])

    def play(self, verbose=True, elves_loss=False):
        """
        Play the game until the battle ends

        Parameters
        ----------
        verbose: bool
            if True, will print out the number of each round
        elves_loss: bool
            if True, will raise an ElfLossException when the first elf dies

        Raises
        ------
        ElfLossException
            if an Elf dies
        """
        nround = 1
        neleves = len(self.elves)
        while len(self.goblins) > 0 and len(self.elves) > 0:
            lst = sorted(self.goblins_and_elves)
            for si, source in enumerate(lst):
                if source not in self.goblins_and_elves:
                    continue
                moveable = False
                attack = False
                for neigh in self.graph.neighbors(source):
                    if isinstance(source, Elf) and isinstance(neigh, Goblin) or \
                       isinstance(source, Goblin) and isinstance(neigh, Elf):
                        attack = True
                    elif isinstance(neigh, Space):
                        moveable = True
                if attack:
                    source.attack()
                elif moveable:
                    source.move()
                if elves_loss and len(self.elves)<neleves:
                    raise ElfLossException("Elf died!")
                if si < len(lst)-1 and (len(self.goblins) == 0 or len(self.elves) == 0):
                    return nround-1
            nround += 1
            if verbose:
                print(nround, len(self.goblins), len(self.elves))
                sys.stdout.flush()
        return nround-1

    def print(self):
        """ Print an ASCII representation of the world
        """
        for row in self.grid.T:
            print("".join(row))

    def remove(self, obj):
        """ Remove an Elf or a Goblin from the world and replace it with a Space object
        """
        space = Space(obj.x, obj.y, self)

        self.grid[obj.x, obj.y] = '.'
        self.spaces[(obj.x, obj.y)] = space

        neigh = list(self.graph.neighbors(obj))
        self.graph.add_node(space)
        for node in neigh:
            self.graph.remove_edge(obj, node)
            self.graph.add_edge(space, node, weight=self._edge_weight(space, node))
        self.graph.remove_node(obj)

        if isinstance(obj, Elf):
            del self.elves[(obj.x, obj.y)]
        elif isinstance(obj, Goblin):
            del self.goblins[(obj.x, obj.y)]
        self.goblins_and_elves.remove(obj)

    def route(self, source, target, min_len):
        """ Find the shortest route from source to target that starts first in reading order
        """
        path_len = None
        first_nodes = []
        if min_len < len(nx.shortest_path(self.graph, source, target, weight="weight")) -1:
            raise Exception("Not the shortest path")
        for path in nx.all_shortest_paths(self.graph, source, target, weight="weight"):
            possible = all([isinstance(node, Space) for node in path[1:-1]])
            if possible:
                path_len = len(path) - 1
                first_nodes.append(path[1])
        if path_len is not None:
            return sorted(first_nodes)[0], path_len
        raise Exception("Path not possible!")

    def swap(self, a, b):
        """ Swap the places of two objects, i.e. moving them in the world
        """
        neigha = list(self.graph.neighbors(a))
        neighb = list(self.graph.neighbors(b))
        for node in neigha:
            if node is b:
                continue
            self.graph.remove_edge(a, node)
            self.graph.add_edge(b, node, weight=self._edge_weight(b, node))
        for node in neighb:
            if node is a:
                continue
            self.graph.remove_edge(b, node)
            self.graph.add_edge(a, node, weight=self._edge_weight(a, node))

        tagb = self.grid[b.x, b.y]
        self.grid[b.x, b.y] = self.grid[a.x, a.y]
        self.grid[a.x, a.y] = tagb

        if isinstance(a, Elf):
            self._update_dict(a.x, a.y, b.x, b.y, self.elves)
        elif isinstance(a, Goblin):
            self._update_dict(a.x, a.y, b.x, b.y, self.goblins)

        if isinstance(b, Space):
            self._update_dict(b.x, b.y, a.x, a.y, self.spaces)
        elif isinstance(b, Elf):
            self._update_dict(b.x, b.y, a.x, a.y, self.elves)
        elif isinstance(b, Goblin):
            self._update_dict(b.x, b.y, a.x, a.y, self.goblins)

        bx = b.x
        by = b.y
        b.x = a.x
        b.y = a.y
        a.x = bx
        a.y = by

    def _add_edges(self):
        for x in range(self.grid.shape[0]):
            for y in range(self.grid.shape[1]):
                source = self._object(x, y)
                if isinstance(source, Wall):
                    continue
                if x < self.grid.shape[0]-1:
                    target = self._object(x + 1, y)
                    if not isinstance(target, Wall):
                        self.graph.add_edge(source, target, weight = self._edge_weight(source, target))
                if y < self.grid.shape[1]-1:
                    target = self._object(x, y + 1)
                    if not isinstance(target, Wall):
                        self.graph.add_edge(source, target, weight=self._edge_weight(source, target))

    def _object(self, x, y):
        if self.grid[x, y] == 'G':
            return self.goblins[(x, y)]
        elif self.grid[x, y] == 'E':
            return self.elves[(x, y)]
        elif self.grid[x, y] == '#':
            return self.walls[(x, y)]
        elif self.grid[x, y] == '.':
            return self.spaces[(x, y)]

    def _update_dict(self, oldx, oldy, newx, newy, dict_):
        obj = dict_[(oldx, oldy)]
        dict_[(newx, newy)] = obj
        del dict_[(oldx, oldy)]

    @staticmethod
    def _edge_weight(source, target):
        if isinstance(source, Space) and isinstance(target, Space):
            return 1
        else:
            return 1E8
