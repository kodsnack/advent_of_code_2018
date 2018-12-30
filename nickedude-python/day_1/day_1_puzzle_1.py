def main():
    answer = 0
    inputs = []
    inputFile = open("day_1_puzzle_1_input.txt")
    inputLines = inputFile.readlines()

    for line in inputLines:
        answer += int(line)

    print("Answer: " + str(answer))

if __name__ == "__main__":
    main()