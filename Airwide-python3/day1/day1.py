with open("input.txt") as input_file:
    input_list = [int(line.strip()) for line in input_file]

# Part 1
print("Part 1 answer: {}".format(sum(input_list)))

# Part 2
result = [0]
class Found(Exception): pass
try:
    while True:
        for each in input_list:
            freq = result[-1] + each
            if freq in result:
                raise Found
            result.append(freq)

except Found:
    print("Part 2 answer: {}".format(freq))
