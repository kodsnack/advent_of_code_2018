/* Advent of code 2018, day 5, part 1 in ANSI REXX */
line = 'dabAcCaCBAcCcaDA'
line = linein('day5.txt')
do until \found
	found = 0
	do # = 1 to length(line)
		parse var line . =(#) a +1 b +1 .
		if bitxor(a,b) == '20'x then do
			found = 1
			leave
		end
	end
	line = delstr(line, #, 2)
end
say length(line)