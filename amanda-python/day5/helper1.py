def lengthOfReactedPolymer(polymer):
    deletedChars = False
    while True:
        deletedChars = False
        for index in range(len(polymer) - 1):
            if index >= len(polymer) - 1:
                break
            char1 = polymer[index]
            char2 = polymer[index + 1]
            # First check if the characters are the same letter
            if(char1.upper() == char2.upper()):
                # Then check if the characters are opposite case
                if(char1 != char2):
                    polymer = polymer[0:index] + polymer[index + 2:]
                    deletedChars = True
        if deletedChars == False:
            break
    return len(polymer)

def inputFileToString(file):
    inputString = ""
    with open(file,'r') as input:
        for line in input:
            inputString = "".join(line)
    return inputString