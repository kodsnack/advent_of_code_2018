/* Advent of code 2018, day 8, part 1 in ANSI REXX */
fn = 'day8.txt'
line = linein(fn)
say node()
exit

node: procedure expose line
parse var line children metadata line
o = 0
do children
	o = o + node()
end
do metadata
	parse var line data line
	o = o + data
end
return o