def main():
    size = 1000
    rectangle = []

    for i in range(0, size):
        rectangle.append([])

        for j in range(0, size):
            rectangle[i].append([])

    inputFile = open("day_3_puzzle_1_input.txt")
    candidates = set()

    for line in inputFile.readlines():
        claim = line.split(" ")
        claimId = int(claim[0][1:])
        coordinates = claim[2].split(",")
        x = int(coordinates[0])
        y = int(coordinates[1][:-1])
        measurements = claim[3].split("x")
        width = int(measurements[0])
        height = int(measurements[1])
        candidate = True

        for i in range(y,y+height):
            for j in range(x,x+width):
                if(rectangle[i][j] != []):
                    candidate = False

                    for otherId in rectangle[i][j]:
                        if(otherId in candidates):
                            candidates.remove(otherId)

                rectangle[i][j].append(claimId)

        if(candidate):
            candidates.add(claimId)

    print("Answer: " + str(candidates))


if __name__ == "__main__":
    main()