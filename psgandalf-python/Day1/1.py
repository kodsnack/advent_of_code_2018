sum = 0
file = open('input.txt', 'r')
input = file.readlines()
for lines in input:
  row = lines.strip(' ')
  sum += int(row)
print (sum)