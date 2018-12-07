/* REXX */
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
say v h t b

size = 0
do y = 0 to b
	do x = 0 to h + 1
		if sum_delta(x,y) < 10000 then size = size + 1
	end
end
say size
exit

delta:
arg x1,y1,x2,y2
return abs(x1 - x2) + abs(y1 - y2)

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

show: procedure expose map. v h t b
do y = 0 to b
	line = ''
	do x = 0 to h + 1
		line = line || map.y.x
	end
	say line
end