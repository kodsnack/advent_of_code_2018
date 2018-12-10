/* Advent of code 2018, day 6, part 1 in ANSI REXX */
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

points. = 0
max_hits = 0
do nr = 1 to queued()
	pull ox oy
	hits = 0
	do y = 0 to b
		do x = 0 to h + 1
			od = delta(x,y,ox,oy)
			min_d = od + 1
			do queued()
				pull rx ry
				queue rx ry
				d = delta(x,y,rx,ry)
				min_d = min(min_d, d)
			end
			if od < min_d then do
				points.y.x = nr
				hits = hits + 1
			end
			if od = min_d then points.y.x = .
		end
	end
	out.1.1 = v;  out.1.2 = oy
	out.2.1 = h;  out.2.2 = oy
	out.3.1 = ox; out.3.2 = t
	out.4.1 = ox; out.4.2 = b
	do # = 1 to 4
		od = delta(out.#.1,out.#.2,ox,oy)
		min_d = b + h
		do queued()
			pull rx ry
			queue rx ry
			d = delta(v,oy,rx,ry)
			min_d = min(min_d, d)
		end
		if od < min_d then do
			hits = 0
			leave #
		end
	end
	max_hits = max(hits, max_hits)
	queue ox oy
end

say max_hits
exit

delta:
arg x1,y1,x2,y2
return abs(x1 - x2) + abs(y1 - y2)
