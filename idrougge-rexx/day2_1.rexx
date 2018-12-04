/* Advent of code 2018, day 2, part 1 in ANSI REXX */
fn = 'day2.txt'
twos = 0
threes = 0
do while lines(fn)
	id = linein(fn)
	two = 0
	three = 0
	do i = 1 to length(id)
		letter = substr(id, i, 1)
		diff = length(id) - length(space(translate(id,,letter),0))
		if diff = 2 then two = 1
		if diff = 3 then three = 1
	end
	twos = twos + two
	threes = threes + three
end
say twos * threes