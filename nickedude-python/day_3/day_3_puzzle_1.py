def main():
    size = 1000
    rectangle = []

    for i in range(0, size):
        rectangle.append([])

        for j in range(0, size):
            rectangle[i].append(0)

    inputFile = open("day_3_puzzle_1_input.txt")

    for line in inputFile.readlines():
        claim = line.split(" ")
        coordinates = claim[2].split(",")
        x = int(coordinates[0])
        y = int(coordinates[1][:-1])
        measurements = claim[3].split("x")
        width = int(measurements[0])
        height = int(measurements[1])

        for i in range(y,y+height):
            for j in range(x,x+width):
                rectangle[i][j] += 1

    count = 0

    for i in range(0, size):
        for j in range(0, size):
            if(rectangle[i][j] > 1):
                count += 1

    print("Answer: " + str(count))


if __name__ == "__main__":
    main()