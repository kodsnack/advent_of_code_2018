from collections import Counter;z=list(zip(*map(lambda x: (1 if 2 in x else 0, 1 if 3 in x else 0), (Counter(l).values() for l in open("input/december02.input")))));print(sum(z[0])*sum(z[1]))

from collections import Counter
a=b=0
for l in open("input/december02.input"):
    v=Counter(l).values()
    if 2 in v:
        a+=1
    if 3 in v:
        b+=1
print(a*b)
