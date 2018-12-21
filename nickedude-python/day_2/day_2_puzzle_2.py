def hammingDistance(s1, s2):
    dist = 0
    for i in range(0,len(s1)):
        if(s1[i] != s2[i]):
            dist += 1

    return dist

def main():
    inputFile = open("day_2_puzzle_1_input.txt")
    inputs = []

    for line in inputFile.readlines():
        inputs.append(line)

    mini = -1
    minj = -1
    minDist = (2 ** 32) - 1

    for i in range(0,len(inputs)):
        for j in range(0,len(inputs)):
            if(i != j):
                dist = hammingDistance(inputs[i], inputs[j])

                if(dist < minDist):
                    minDist = dist
                    mini = i
                    minj = j

    answer = ""
    s1 = inputs[mini]
    s2 = inputs[minj]
    print(minDist)
    print(s1)
    print(s2)

    for i in range(0,len(s1)):
            s1char = s1[i]
            s2char = s2[i]

            if(s1char == s2char):
                answer = answer + s1char

    print("Answer: " + answer)


if __name__ == "__main__":
    main()