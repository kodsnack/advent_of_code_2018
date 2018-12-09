from aocbase import readInput
import re
import array
#493 players; last marble is worth 71863 points

def insert(circ, sz, p, v):
    for i in range(sz, p, -1):
        circ[i] = circ[i-1]
    circ[p] = v

def delete(circ, sz, p):
    for i in range(p, sz-1):
        circ[i] = circ[i+1]

class marble:
    def __init__(self, val, prv, nxt):
        self.v, self.p, self.n = val, prv, nxt
        if val==0:
            self.n = self
            self.p = self
        else:
            self.n.p = self
            self.p.n = self
    def remove(self):
        self.p.n = self.n
        self.n.p = self.p
    def counter(self, n):
        while n>0:
            self = self.p
            n -= 1
        return self
    def clock(self, n):
        while n>0:
            self = self.n
            n -= 1
        return self
    def print(self):
        i = self
        while True:
            print(i.v, end=' ')
            i = i.n
            if i==self:
                break
        print()

def marbles(marb, elves):
    cur = first = marble(0, None, None)
    m, e = 1, 0
    score = dict()
    while m < marb:
        if m % 23 == 0:
            score[e+1] = score.get(e+1, 0) + m
            p = cur.counter(7)
            score[e+1] = score[e+1] + p.v
            cur = p.n
            p.remove()
        else:
            p1 = cur.clock(1)
            p2 = p1.clock(1)
            cur = marble(m, p1, p2)
        #print(e+1, end=":")
        #first.print()
        e = (e + 1)%elves
        m += 1
    return score

i = marbles(71863, 493)
print("Solution to day 9 part 1:", max(i.values()))
i = marbles(71863*100, 493)
print("Solution to day 9 part 2:", max(i.values()))
