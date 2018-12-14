#PART 1
# define a function to add a list of numbers to a running total
def drift(x):
    freq = 0
    for el in x:
        freq += el
    return freq

# use input to read the freq drifts (cut and paste in), convert to integers and put in a list
listx = list(map(int, input().split()))

# print(listx)

#do the math
print(drift(listx))

#PART 2
# track all results and report the first duplicate value for 'freq'
def drift2(x):
    freq = 0
    loop=1
    loopcount = 0 
    while loop: 
        for el in x:
            freq += el
            #print(el, freq)
            if freq not in dups:
                dups.append(freq) 
            else: 
                return freq
        loopcount +=1
        if loopcount > 150:
            loop = 0 


#create an empty list to store freq values in 
dups = [0]

#get space separated list of numbers and convert to a list
x = list(map(int, input().split()))

#print(x)
print(drift2(x))