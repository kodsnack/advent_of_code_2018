import sys

# Read input
input = sys.stdin.read().strip()
input = input.split(' ')

players = int(input[0])
last_marble = int(input[6])

def winning_score(players, last_marble):
    # Initialize linked list
    root = {'value':0, 'next':None, 'prev':None}
    root['next'] = root
    root['prev'] = root

    # Set current node to ring root
    node = root
    # Player scores
    score = [0 for p in range(players)]
    for marble in range(1,last_marble+1):
        # If marble multiple of 23
        if marble % 23 == 0:
            # Give player marble score
            score[marble % players] += marble
            # Move -7 hops away
            node = node['prev']['prev']['prev']['prev']['prev']['prev']['prev']
            # Give player score
            score[marble % players] += node['value']
            # Pop marble
            node['next']['prev'] = node['prev']
            node['prev']['next'] = node['next']
            node = node['next']
        else:
            # Move one marble forward
            node = node['next']
            # Insert new marble
            new_node = {'value':marble, 'next':node['next'], 'prev':node}
            node['next']['prev'] = new_node
            node['next'] = new_node
            node = new_node
    return max(score)

print('Part 1:', winning_score(players,last_marble))
print('Part 2:', winning_score(players,100*last_marble))


