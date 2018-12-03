sum = 0

with open('input.txt','r') as input:
    for line in input:
        if (line[0] == '+'):
            sum += int(line[1:])
        else:
            sum -= int(line[1:])
            
print(sum)