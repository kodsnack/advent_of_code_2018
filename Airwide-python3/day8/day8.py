#!/usr/bin/env python3
with open("input.txt") as input_file:
    puzzleInput = [line.strip().split() for line in input_file]
    puzzleInput = list(map(int, puzzleInput[0]))
# print('Puzzle input:', puzzleInput)

headerList = []
metaList = []
def get_node_data(puzzleInput, nodes=1):
    pos = 0
    valueList = []
    for eachChild in range(nodes):
        header = puzzleInput[pos:pos + 2]
        childNodes = header[0]
        metaEntries = header[1]
        value = 0
        if childNodes == 0:
            pos += 2 + metaEntries                                                   
            meta = puzzleInput[pos - metaEntries:pos]
            valueList.append(sum(meta))
            metaList.append(meta)
        else:
            newPos, childValue = get_node_data(puzzleInput[pos + 2:], childNodes)
            pos += newPos + metaEntries
            meta = puzzleInput[pos - metaEntries:pos]
            metaList.append(meta)
            for each in meta:
                try:
                    value += childValue[each - 1]
                except IndexError:
                    pass
            valueList.append(value)
        headerList.append(header)
    return pos + 2, valueList

pos, childValue = get_node_data(puzzleInput)
# print('Headers:', headerList)
# print('Metameta:', metaList)
result1 = sum(sum(metaList, []))
print('Day 8 part 1 result: {}'.format(result1))
print('Day 8 part 2 result: {}'.format(childValue[0]))
