import sys

# Read input
input = sys.stdin.read()
input = input.strip().split('\n')

# Part 1
frequency_change = [int(fc[1:]) if fc[0] == '+' else -int(fc[1:]) for fc in input]
print('Part 1: ' + str(sum(frequency_change)))

# Part 2
reached_frequencies = set()
i = 0
frequency = 0
# Loop as long as frequency not already reached
while frequency not in reached_frequencies:
    # Add reached to set
    reached_frequencies.add(frequency)
    # Calculate next frequency
    frequency += frequency_change[i]
    # Increment index
    i = (i + 1) % len(frequency_change)
print('Part 2: ' + str(frequency))
