import sys

# Read input
input = sys.stdin.read().strip()
recipes = '37'
i,j = 0,1

# Loop until input pattern appears
while input not in recipes[-len(input)-1:]:
    # Calculate next recipe
    recipes += str(int(recipes[i]) + int(recipes[j]))
    # Increment indices
    i = (i + int(recipes[i]) + 1) % len(recipes)
    j = (j + int(recipes[j]) + 1) % len(recipes)

# Ten digits after input number of recipes
print('Part 1:', recipes[int(input):int(input)+10])

# Length of recipes left of input pattern
print('Part 2:', recipes.index(input))
