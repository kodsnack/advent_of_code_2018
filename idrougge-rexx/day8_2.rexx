/* REXX */
fn = 'day8.txt'
line = linein(fn)
say node()
exit

node: procedure expose line
parse var line children metadata line
o = 0
c. = 0
do child = 1 for children
	c.child = node()
end
do metadata
	parse var line data line
	if children = 0
	then o = o + data
	else o = o + c.data
end
return o
