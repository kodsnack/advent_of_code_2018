/* Advent of code 2018, day 10, part 1 & 2 in ANSI REXX */
fn = 'day10.txt'

parse value 0 0 0 0 with l r t b /* Quick init of multiple variables */

/* Read and parse input into the queue */
do while lines(fn)
	parse value linein(fn) with . '<' px',' py'>' . '<' vx',' vy'>' .
	px = strip(px); py = strip(py)
	vx = strip(vx); vy = strip(vy)
	l = min(l, px); r = max(r, px)
	t = min(t, py); b = max(b, py)
	queue px py vx vy
end

do times = 0 while 1 > 0
	l_ = l; r_ = r /* Save l & r for comparison */
	parse value r l b t with l r t b /* Swap l/r and t/b */
	do queued()
		pull px py vx vy
		px = px + vx
		py = py + vy
		queue px py vx vy
		l = min(l, px); r = max(r, px)
		t = min(t, py); b = max(b, py)
	end
	if r_ < r then leave /* Break loop if distance between max/min coordinates grows */
end times

parse value r l b t with l r t b /* Swap l/r and t/b */
/* Go back one step */
do queued()
	pull px py vx vy
	px = px - vx
	py = py - vy
	queue px py vx vy
	l = min(l, px); r = max(r, px)
	t = min(t, py); b = max(b, py)
end

call show
say times
exit

show: procedure expose t b l r
map. = '.'
do queued()
	pull px py vx vy
	queue px py vx vy
	map.py.px = '#'
end
do y = t to b
	line = ''
	do x = l to r
		line = line || map.y.x
	end x
	say line
end y
return
