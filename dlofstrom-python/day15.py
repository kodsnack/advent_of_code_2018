import sys
import re

# Read input
input = sys.stdin.read().strip()

stepbystep = """#######   
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"""

test1 = """#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"""

test2 = """#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"""

test3 = """#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######"""

test4 = """#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"""

test5 = """#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########"""


def generate_map(input):
    input = input.split('\n')
    input = [list(l) for l in input]
    
    elves = {}
    goblins = {}
    walls = set()
    
    for y,l in enumerate(input):
        for x,v in enumerate(l):
            if v == 'E':
                elves[(y,x)] = 200
            elif v == 'G':
                goblins[(y,x)] = 200
            elif v == '#':
                walls.add((y,x))
    return elves, goblins, walls

def pretty(elves, goblins, walls):
    sy,sx = max(walls)
    s = ''
    for y in range(sy+1):
        row = ''
        hp = ''
        for x in range(sx+1):
            if (y,x) in elves:
                row += 'E'
                if hp != '':
                    hp += ', '
                hp += 'E('+str(elves[(y,x)])+')'
            elif (y,x) in goblins:
                row += 'G'
                if hp != '':
                    hp += ', '
                hp += 'G('+str(goblins[(y,x)])+')'
            elif (y,x) in walls:
                row += '#'
            else:
                row += '.'
        s += row + '   ' + hp
        if y < sy:
            s += '\n'
    return s
            
def shortest_path(p1, p2, elves, goblins, walls):
    queue = [(p1,())]
    visited = set()
    while queue:
        p,path = queue.pop(0)
        y,x = p
        # If done
        if p == p2:
            return path + (p,)
        # Generate next steps
        ps = [(y-1,x),(y,x-1),(y,x+1),(y+1,x)]
        for pn in ps:
            if pn not in visited and pn not in walls and pn not in elves and pn not in goblins:
                # Add to queue
                queue.append((pn,path+(p,)))
                visited.add(pn)
    return ()


#print(pretty(elves, goblins, walls))

#print(shortest_path((1,1),(1,2),elves,goblins,walls))

def solve(elves, elves_attack, goblins, goblins_attack, walls, print_frequency):
    # Loop until no elves or no goblins left
    rounds = 0
    while elves and goblins:
        order = sorted(list(elves)+list(goblins))
        while order:
            # Select creature
            p = order.pop(0)
            #print(p)
            t = ''
            hp = 0
            if p in elves:
                t = 'E'
                hp = elves.pop(p)
                friends = elves
                enemies = goblins
                ap = elves_attack
            elif p in goblins:
                t = 'G'
                hp = goblins.pop(p)
                friends = goblins
                enemies = elves
                ap = goblins_attack
            else:
                continue
            #print(t, p, hp)
                
            # Select targets
            targets = list(enemies)
            #print(targets)
            
            # In range
            inrange = set(a for yt,xt in targets for a in [(yt-1,xt),(yt,xt-1),(yt,xt+1),(yt+1,xt)])
            inrange = [a for a in inrange if a not in elves and a not in goblins and a not in walls]
            #print(inrange)
            
            # Is reachable
            paths = {pr:shortest_path(p,pr,elves,goblins,walls) for pr in inrange}
            reachable = [pr for pr in inrange if paths[pr]]
            pathlen = {pr:len(paths[pr]) for pr in reachable}
            #print(reachable)

            #Shortest
            shortest = [pr for pr in reachable if len(paths[pr]) == min(pathlen.values())]
            #print(shortest)

            if shortest:
                #Selected
                selected = min(shortest)
                
                #Take step
                if pathlen[selected] > 1:
                    #First step is current, so take second
                    p = paths[selected][1]
            friends[p] = hp

            #Attack
            adjacent = [e for e in [(p[0]-1,p[1]),(p[0],p[1]-1),(p[0],p[1]+1),(p[0]+1,p[1])] if e in enemies]
            if adjacent:
                selected = min(adjacent, key=lambda k: enemies[k])
                enemies[selected] -= ap
                if enemies[selected] <= 0:
                    enemies.pop(selected)

            if not elves or not goblins:
                break

        if not order:
            rounds += 1

        if print_frequency == 2:
            print('\n',rounds,'\n',pretty(elves, goblins, walls))

    if print_frequency == 1:
        print(rounds,'\n',pretty(elves, goblins, walls))
        
    return (rounds, sum(elves.values()), sum(goblins.values()), len(elves), len(goblins))

#print(elves)
#print(goblins)

"""
elves,goblins,walls = generate_map(stepbystep)
r,e,g = solve(dict(elves), 3, dict(goblins), 3, walls,2)
print('Step by step:', r*(e+g))

elves,goblins,walls = generate_map(test1)
r,e,g = solve(dict(elves), 3, dict(goblins), 3, walls,1)
print('Test 1:', r*(e+g))

elves,goblins,walls = generate_map(test2)
r,e,g = solve(dict(elves), 3, dict(goblins), 3, walls,1)
print('Test 2:', r*(e+g))

elves,goblins,walls = generate_map(test3)
r,e,g = solve(dict(elves), 3, dict(goblins), 3, walls,1)
print('Test 3:', r*(e+g))

elves,goblins,walls = generate_map(test4)
r,e,g = solve(dict(elves), 3, dict(goblins), 3, walls,1)
print('Test 4:', r*(e+g))

elves,goblins,walls = generate_map(test5)
r,e,g = solve(dict(elves), 3, dict(goblins), 3, walls,1)
print('Test 5:', r*(e+g))
"""

elves,goblins,walls = generate_map(input)
r,e,g,el,gl = solve(dict(elves), 3, dict(goblins), 3, walls,1)
print('Part 1:', r*(e+g))

attack = 3
while el < len(elves):
    attack += 1
    print(attack)
    r,e,g,el,gl = solve(dict(elves), attack, dict(goblins), 3, walls,1)
print('Part 2:', r*(e+g))
