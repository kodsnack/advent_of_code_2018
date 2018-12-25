from aocbase import readInput

def parse(s):
    l = []
    for line in s.splitlines():
        l.append(list(map(int,line.split(","))))
    return l

def fitsInConstellation(star1, con):
    for star2 in con:
        if sum(map(lambda x,y:abs(x-y), star1, star2)) < 4:
            return True
    return False

def mergeConstellations(constellations, toMerge):
    newConstellation = []
    while len(toMerge)>0:
        theConstellation = toMerge.pop()
        newConstellation.extend(theConstellation)
        constellations.remove(theConstellation)
    constellations.append(newConstellation)

def clusterStars(stars):
    constellations = []
    for star in stars:
        constellations.append([star])
        matchingConstellations = []
        for constellation in constellations:
            if fitsInConstellation(star, constellation):
                matchingConstellations.append(constellation)
        mergeConstellations(constellations, matchingConstellations)
    return constellations

stars = parse(readInput())
constellations = clusterStars(stars)
print("Solution to day 25 part 1:",len(constellations))