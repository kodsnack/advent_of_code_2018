from helper1 import lengthOfReactedPolymer
from string import ascii_lowercase
from string import *
from sys import maxsize

def getOptimizedAlphabet(inputString):
    ALPHABET = list(ascii_lowercase)
    optimizedAlphabet = list(ascii_lowercase)

    for letter in ALPHABET:
        if letter not in inputString and letter.upper() not in inputString:
            del optimizedAlphabet[optimizedAlphabet.index(letter)]

    return optimizedAlphabet

def getSmallestPolymerLength(inputAlphabet, inputString):
    smallestPolymerLength = maxsize
    smallestPolymerLetter = ''
    smallestPolymer = ''

    for letter in inputAlphabet:
        print('Checking length when removing ' + letter.upper() + '/' + letter + '...')
        # Remove all instances of letter from the input string
        reactedString = inputString
        for index, char in enumerate(inputString):
            if char.lower() == letter:
                reactedString = reactedString.replace(letter, '')
                reactedString = reactedString.replace(letter.upper(), '')
        # Then find the polymer length, and save the length and letter if 
        # it is the lowest so far
        length = lengthOfReactedPolymer(reactedString)
        if length < smallestPolymerLength:
            smallestPolymerLength = length
            smallestPolymerLetter = letter
            smallestPolymer = reactedString

    return smallestPolymerLength