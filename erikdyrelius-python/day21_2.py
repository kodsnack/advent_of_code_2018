r5 = 0
r0 = 0
cache = []
first = True
while True:
    r4 = r5 | 65536
    r5 = 13284195
    while True:
        r3 = r4 & 255
        r5 += r3
        r5 &= 16777215
        r5 *= 65899
        r5 &= 16777215
        if r4 < 256:
            break
        r4 //= 256
    if first:
        first = False
        print("Solutions to day 21 part 2:", r5)
    elif r5 in cache:
        if r5 != cache[-1]:
            print("Solutions to day 21 part 2:", cache[-1])
        else:
            print("Solutions to day 21 part 2:", cache[-2])
        break
    cache.append(r5)
