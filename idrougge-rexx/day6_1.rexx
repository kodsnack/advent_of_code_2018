/* REXX */
fn = 'day6b.txt'
map. = '.'
h = 0; b = 0
do while lines(fn)
	parse value linein(fn) with x',' y .
	say x y
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
call show

say minimum_delta(2, 2)
say minimum_delta(3, 4)
say minimum_delta(2, b)

do y = 0 to b
	do x = 0 to h
		say x y '>' minimum_delta(x,y)
	end
end

do queued()
	pull rx ry
	queue rx ry
	do y = 0 to b
		do x = 0 to h
			say x':'y '>' delta(x,y,rx,ry)
		end
	end	
end

points. = 0
hits = 0
do queued()
pull ox oy
do y = 0 to b
	do x = 0 to h + 1
		od = delta(x,y,ox,oy)
		min_d = od + 1
		do queued()
			pull rx ry
			queue rx ry
			say 'od:' od
			d = delta(x,y,rx,ry)
			say x':'y '<>' rx':'ry '->' d
			min_d = min(min_d, d)
		end
		say 'min_d:' min_d 'od:' od
		if od < min_d then do
			points.y.x = 'f'
			/* points.oy.ox = points.oy.ox + 1 */
			hits = hits + 1
		end
		if od = min_d then points.y.x = .
	end
end
queue ox oy
end
call show
say rx ry
say ox oy
say hits
exit

delta:
arg x1,y1,x2,y2
return abs(x1 - x2) + abs(y1 - y2)

minimum_delta: procedure expose b h
arg rx,ry
min_x = h
min_y = b
min_d = b + h
do queued()
	pull x y
	queue x y
	dx = abs(rx - x)
	dy = abs(ry - y)
	d = dx + dy
	if d < min_d then do
		min_x = x
		min_y = y
	end
	min_d = min(min_d, d)
	/* say x y '>' dx dy '>' dx + dy */
end
say min_x min_y min_d
return min_d

show: procedure expose map. points. v h t b
do y = 0 to b
	line = ''
	do x = 0 to h + 1
		line = line || points.y.x
	end
	say line
end