/* Advent of code 2018, day 4, parts 1 & 2 in ANSI REXX */
in = 'day4.txt'
fn = 'sorted.txt'
'sort' in '>' fn
guards. = 0
nr_of_guards = 0
do while lines(fn)
	line = linein(fn)
	parse var line '['y'-'mo'-'d h':'mi']' text
	select
		when pos('wakes', text) > 0 then do
			woke = mi - 1
			do i = fell to woke
				guards.nr.i = guards.nr.i + 1
			end
		end
		when pos('falls', text) > 0 then do
			fell = mi
		end
		otherwise do
			parse var text . '#'nr .
			nr_of_guards = max(nr, nr_of_guards)
		end
	end
end

longest_time_slept = 0
the_minute_that_the_sleepiest_guard_slept_the_most_on = -1
the_guard_that_fell_asleep_most_often_on_the_same_minute = -1
the_minute_most_slept_on = 0
max_minutes = 0
do guard = 1 to nr_of_guards
	sleeping = 0
	top = 0
	personal_max = 0
	do minute = 0 to 59
		sleeping = sleeping + guards.guard.minute
		if guards.guard.minute > max_minutes then do
			the_guard_that_fell_asleep_most_often_on_the_same_minute = guard
			the_minute_most_slept_on = minute
		end
		if guards.guard.minute > personal_max then top = minute
		personal_max = max(guards.guard.minute, personal_max)
		max_minutes = max(personal_max, max_minutes)
	end
	if sleeping > longest_time_slept then do
		the_minute_that_the_sleepiest_guard_slept_the_most_on = top
		sleepiest = guard
	end
	longest_time_slept = max(sleeping, longest_time_slept)
end

say 'Part 1:' sleepiest * the_minute_that_the_sleepiest_guard_slept_the_most_on
say 'Part 2:' the_minute_most_slept_on * the_guard_that_fell_asleep_most_often_on_the_same_minute
