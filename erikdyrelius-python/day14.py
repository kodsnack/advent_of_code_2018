inp = 513401
inp = "513401"

board = [3, 7]
elves = [0, 1]

#while len(board) < 10+inp:
#    sm = 0
#    for elf in elves:
#        sm += board[elf]
#    sms = str(sm)
#    for c in sms:
#        board.append(int(c))
#    for i in range(len(elves)):
#        elves[i] = (elves[i] + board[elves[i]] + 1) % len(board)
#    #print(board)
#    #print(elves)
#print(''.join(map(str, board[inp:inp+10])))

il = len(str(inp))
j = 0
while True:
    sm = 0
    for elf in elves:
        sm += board[elf]
    sms = str(sm)
    for c in sms:
        board.append(int(c))
    for i in range(len(elves)):
        elves[i] = (elves[i] + board[elves[i]] + 1) % len(board)
    #print(board)
    #print(elves)
    if ''.join(map(str, board[-il:])) == str(inp):
        j = len(board) - len(str(inp))
        break
    if ''.join(map(str, board[-il-1:-1])) == str(inp):
        j = len(board) - len(str(inp)) - 1
        break
    j += 1
print(board[-10:], j)

print("Solution to day 14 part 1:".format(14.1))
print("Solution to day 14 part 2:".format(14.2))
