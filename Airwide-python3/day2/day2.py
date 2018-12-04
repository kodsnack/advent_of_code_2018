with open("input.txt") as input_file:
    input_list = [line.strip() for line in input_file]

# Part 1
d_sum = 0
t_sum = 0
for line in input_list:
    d_set = set()
    t_set = set()
    doubles = []
    triples = []
    for i in line:
        if i in t_set:
            triples.append(i)
            t_set.remove(i)
            doubles.remove(i)
        elif i in d_set:
            doubles.append(i)
            d_set.remove(i)
            t_set.add(i)
        else:
            d_set.add(i)
        if len(doubles) > 0 and len(triples) > 0:
            break
    if len(doubles) > 0:
        d_sum += 1
    if len(triples) > 0:
        t_sum += 1

print("Part 1 answer: {}".format(d_sum * t_sum))

# Part 2
matched_ids = []
for id1 in input_list:
    for id2 in input_list:
        matched_chars = [] 
        for i in range(len(id2)):
            if id2[i] == id1[i]:
                matched_chars.append(id2[i])
        if len(matched_chars) + 1 == len(id1):
            id_string = ''.join(matched_chars)
            if not id_string in matched_ids:
                matched_ids.append(id_string)

print("Part 2 answer:")
for each in matched_ids:
    print(each)
