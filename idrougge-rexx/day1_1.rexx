/* Advent of code 2018, day 1, part 1 in ANSI REXX */
freq = 0
do while lines('day1.txt')
	change = linein('day1.txt')
	freq = freq + change
end
say freq