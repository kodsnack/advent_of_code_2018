def main():
    answer = 0
    inputs = []
    inputFile = open("day_1_puzzle_1_input.txt")
    inputLines = inputFile.readlines()

    for line in inputLines:
        inputs.append(int(line))

    seen = set()
    index = 0

    while(True):
        seen.add(answer)
        answer += inputs[index]
        index = (index + 1) % len(inputs)

        if(answer in seen):
            break

    print("Answer: " + str(answer))

if __name__ == "__main__":
    main()