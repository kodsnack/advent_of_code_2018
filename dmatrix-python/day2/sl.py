from collections import Counter

# Part 1
def get(word):
    d = dict(Counter(word)).items()
    return [1 if len({k: v for k, v in d if v == 2}) > 0 else 0,
            1 if len({k: v for k, v in d if v == 3}) > 0 else 0]

frs = 0
scn = 0
with open('input.txt') as f:
    my_lines = f.readlines()
    for line in my_lines:
        lst = get(line)
        frs += lst[0]
        scn += lst[1]
    print(f"First part answer: {frs*scn}")


# Part 2
def check(word1, word2):
    cont = 0
    for i in range(len(word1)):
        if word1[i] != word2[i]:
            cont += 1
            if cont > 1:
                return False
    return True

print("Second part answer: ", end="")
with open('input.txt') as f:
    my_lines = f.readlines()
    for i in range(len(my_lines)):
        for j in range(i + 1, len(my_lines)):
            if(check(my_lines[i], my_lines[j])):
                for k in range(len(my_lines[i])):
                    if(my_lines[i][k] == my_lines[j][k]):
                        print(my_lines[i][k], end='')
