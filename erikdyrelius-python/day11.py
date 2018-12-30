def allPowl(sid):
    d = dict()
    for x in range(1,301):        
        for y in range(1,301):
            rackid = x + 10
            pwl = (rackid*y + sid)*rackid 
            d[(x,y)] = (pwl % 1000)//100 - 5
    return d

def allPowlAllSz(sid, d, szm):
    cache = {}
    for sz in range(1, szm+1):
        for x in range(1, 301-sz+1):
            for y in range(1, 301-sz+1):
                sm = 0
                if sz > 1:
                    b = (sz+1)//2
                    s = sz//2
                    sm = cache[(x, y, b)]+cache[(x+b, y, s)]+cache[(x, y+b, s)]+cache[(x+s, y+s, b)]
                    if s!=b:
                        sm -= d[(x+s, y+s)]
                else:
                    sm = d[(x, y)]
                cache[(x,y,sz)] = sm
    return cache

def findMax(cache, sz=None):
    mxv = 0
    for key, v in cache.items():
        if sz and sz != key[2]:
            continue
        if mxv < v:
            mxv = v
            mxx, mxy, mxs = key
    if sz:
        return mxx, mxy
    else:
        return mxx, mxy, mxs

sid = 8772
d = allPowl(sid)
cache = allPowlAllSz(sid, d, 300)
print("Solution to day 11 part 1: {}".format(','.join(map(str, findMax(cache, 3)))))
print("Solution to day 11 part 2: {}".format(','.join(map(str, findMax(cache)))))
