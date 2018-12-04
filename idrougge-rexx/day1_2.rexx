/* Advent of code 2018, day 1, part 2 in ANSI REXX */
fn = 'day1.txt'
freq = 0
log. = 0
do forever
	do while lines(fn)
		change = linein(fn)
		freq = freq + change
		if log.freq = freq then signal done
		log.freq = freq
	end
	call lineout fn
end
done:
say freq
