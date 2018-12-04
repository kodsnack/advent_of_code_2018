/* Advent of code 2018, day 3, part 2 in ANSI REXX */
fn = 'day3.txt'
map. = 0
sub. = 0
do while lines(fn)
	line = linein(fn)
	parse var line '#'id '@' +2 x','y':' +2 w'x'h
	call draw x,y,w,h
	queue id x y w h
end

do while queued() > 0
	pull id x y w h
	if \testsub(x,y,w,h) then leave
end

say id
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

testsub: procedure expose sub.
arg @x, @y, w, h
do y = @y to @y + h - 1
	do x = @x to @x + w - 1
		if sub.y.x then return 1
	end
end
return 0
