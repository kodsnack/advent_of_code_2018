/* Advent of code 2018, day 5, part 1 in ANSI REXX */
line = linein('day5.txt')
alfa = xrange('a','z')
shortest = length(line)
do length(alfa)
	parse var alfa a +1 alfa
	a = a||translate(a)
	shortened = space(translate(line,,a),0)
	shortened = react(shortened)
	shortest = min(shortest, shortened)
end
say shortest
exit

react: procedure
parse arg line
do until \found
	do # = 1 to length(line)
		parse var line . =(#) a +1 b +1 .
		if bitxor(a,b) == '20'x then do
			found = 1
			leave
		end
		found = 0
	end
	line = delstr(line, #, 2)
end
return length(line)
