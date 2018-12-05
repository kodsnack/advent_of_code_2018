from aocbase import readInput
import re

inp = readInput()
def estrip(inp):
    change = True
    s = inp
    while change:
        change = False
        i = 0
        n = []
        while i < len(s)-1:
            c1, c2 = s[i], s[i+1]
            if (('a'<=c1<='z' and ord(c1)-32 == ord(c2)) or
                ('A'<=c1<='Z' and ord(c1)+32 == ord(c2))):
                i = i + 1
                change = True
            else:
                n.append(c1)
            i = i + 1
        if i == len(s)-1:
            n.append(s[-1])
        s = ''.join(n)
    return s
s4 = estrip(inp)
print("Solution to day 5 part 1:", len(s4))
s = inp
s2 = set(s4.lower())
m = len(s)
for i in s2:
    s3 = ''.join(map(lambda x,y=i:'' if x==y or ord(x)+32 == ord(y) else x, s4))
    es = estrip(s3)
    m = min(m, len(es))
    print(i, len(es))
print("Solution to day 5 part 2:", m)
