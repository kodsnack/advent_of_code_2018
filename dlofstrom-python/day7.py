import sys

# Read input
input = sys.stdin.read()

# Extract data
input = input.strip().split('\n')
input = [l.split(' ') for l in input]
input = [(l[1],l[7]) for l in input]

# Generate order dictionary, key must be finished before values
order = {}
for k,v in input:
    order[k] = order.get(k,[]) + [v]
    order[v] = order.get(v,[])

# All steps to be completed
steps_left = set(order)
final_order = ''
while steps_left:
    # Determine available steps
    available = steps_left.difference(set([s for step in steps_left for s in order[step]]))
    # Pick alphabetically and add to final order
    final_order += min(available)
    # Remove chosen from steps left
    steps_left.remove(final_order[-1])

print('Part 1:', final_order)


# Specify how many workers and extra seconds
workers = 5
extra_seconds = 60

# Worker structure (second when complete, step)
workers = {k:(0, '') for k in range(workers+1)}

# All steps to be completed
steps_left = set(order)
second = 0
while steps_left:
    # Remove completed steps
    steps_left.difference_update(set([i[1] for w,i in workers.items() if i[0] == second]))

    # Determine available steps
    available = steps_left.difference(set([s for step in steps_left for s in order[step]]))
    # Also remove steps currently in progress
    available.difference_update(set([i[1] for w,i in workers.items()]))
    # Sort alphabetically
    available = sorted(list(available))
        
    # Assign new tasks for workers
    for w,i in workers.items():
        if available and i[0] <= second:
            step = available.pop(0)
            step_time = ord(step) - ord('A') + 1 + extra_seconds
            workers[w] = (second+step_time, step)
        
    # Increment time
    if steps_left:
        second += 1    

print('Part 2:', second)
