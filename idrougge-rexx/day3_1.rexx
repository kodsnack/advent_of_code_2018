/* Advent of code 2018, day 3, part 1 in ANSI REXX */
fn = 'day3.txt'
map. = 0
sub. = 0
do while lines(fn)
	line = linein(fn)
	parse var line id '@' +2 x','y':' +2 w'x'h
	call draw x,y,w,h
end
say sum()
exit

draw: procedure expose map. sub.
arg @x, @y, w, h
do y = @y to @y + h - 1
	do x = @x to @x + w - 1
		if map.y.x then sub.y.x = 1
		map.y.x = 1
	end
end
return

sum: procedure expose sub.
sum = 0
do y = 0 to 1000
	do x = 0 to 1000
		sum = sum + sub.y.x
	end
end
return sum
