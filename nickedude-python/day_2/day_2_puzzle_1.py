def main():
    inputFile = open("day_2_puzzle_1_input.txt")
    triplets = 0
    duplicates = 0

    for line in inputFile.readlines():
        charCount = dict()

        for char in line:
            if(char not in charCount):
                charCount[char] = 0

            charCount[char] += 1

        duplicateFound = False
        tripletFound = False

        for char in charCount.keys():
            if((not duplicateFound) and (charCount[char] == 2)):
                duplicates += 1
                duplicateFound = True

            if((not tripletFound) and (charCount[char] == 3)):
                triplets += 1
                tripletFound = True

    print("Answer: " + str(triplets * duplicates))


if __name__ == "__main__":
    main()