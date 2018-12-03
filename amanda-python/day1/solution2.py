def firstFrequencyReachedTwince(inputFile):
    sum = 0
    pastSums = set()
    pastSums.add(sum)
    
    while(True):
        with open(inputFile,'r') as input:
            for line in input:
                if (line[0] == '+'):
                    sum += int(line[1:])
                else:
                    sum -= int(line[1:])
                if (sum in pastSums):
                    return sum
                pastSums.add(sum)

print(firstFrequencyReachedTwince('input.txt'))