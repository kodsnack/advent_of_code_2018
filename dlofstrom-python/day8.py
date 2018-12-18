import sys

# Read input
input = sys.stdin.read().strip()
input = [int(x) for x in input.split(' ')]

# Sum all metadata entries recursively
def mdsum(data):
    # Get number of children and metadata
    children = data.pop(0)
    metadata = data.pop(0)
    metadata_sum = 0
    # Calculate childrens metadata sum
    for i in range(children):
        metadata_sum += mdsum(data)
    # Add sum of own metadata
    for i in range(metadata):
        metadata_sum += data.pop(0)
    return metadata_sum

print('Part 1:', mdsum(list(input)))

# Calculate node value
def value(data):
    # Get number of children and metadata
    children = data.pop(0)
    metadata = data.pop(0)
    node_value = 0
    children_values = []
    # Calculade value of children
    for i in range(children):
        children_values += [value(data)]
    # Use metadata to either index children or add to value
    for i in range(metadata):
        m = data.pop(0)
        # If no children, add metadata to value
        if children == 0:
            node_value += m
        # If child node exists, add child value to value
        elif m <= children:
            node_value += children_values[m-1]
    return node_value

print('Part 2:', value(list(input)))
