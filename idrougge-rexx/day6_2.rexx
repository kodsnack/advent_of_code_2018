/* Advent of code 2018, day 6, part 2 in ANSI REXX */
fn = 'day6.txt'
map. = '.'
h = 0; b = 0
do while lines(fn)
	parse value linein(fn) with x',' y .
	push x y
	h = max(x,h)
	b = max(y,b)
	map.y.x = '*'
end

v = h; t = b
do queued()
	pull x y
	v = min(v,x)
	t = min(t,y)
	queue x y
end

size = 0
do y = 0 to b
	do x = 0 to h + 1
		if sum_delta(x,y) < 10000 then size = size + 1
	end
end
say size
exit

sum_delta: procedure expose b h
arg rx,ry
d = 0
do queued()
	pull x y
	queue x y
	dx = abs(rx - x)
	dy = abs(ry - y)
	d = d + dx + dy
end
return d
