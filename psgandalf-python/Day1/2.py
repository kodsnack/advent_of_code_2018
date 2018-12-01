sum = 0
results = []
found = False
file = open('input.txt', 'r')
input = file.readlines()
while not found:
    for lines in input:
        results.append(sum)
        row = lines.strip(' ')
        sum += int(row)
        if sum in results:
            found = True
            break
print(sum)
