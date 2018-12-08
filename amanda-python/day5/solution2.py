from helper1 import inputFileToString
from helper2 import getOptimizedAlphabet, getSmallestPolymerLength

inputString = inputFileToString('day5/input.txt')
print('Smallest length is: ' + str(getSmallestPolymerLength(getOptimizedAlphabet(inputString), inputString)))

# Note: Answer for input.txt should be 4840