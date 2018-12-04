/* REXX */
in = 'day4.txt'
fn = 'sorted.txt'
'sort' in '>' fn
guards. = 0
nr_of_guards = 0
do while lines(fn)
	line = linein(fn)
	say line
	parse var line '['y'-'mo'-'d h':'mi']' text
	say d'/'mo 'kl.' h':'mi':' text
	select
		when pos('wakes', text) > 0 then do
			say 'Vakt' nr 'vaknade'
			woke = mi - 1
			do i = fell to woke
				guards.nr.i = guards.nr.i + 1
			end
			say 'Vakt' nr 'sov i' mi - fell 'minuter'
		end
		when pos('falls', text) > 0 then do
			say 'Vakt' nr 'somnade'
			fell = mi
		end
		otherwise do
			parse var text . '#'nr .
			say 'Vakt nr' nr
			nr_of_guards = max(nr, nr_of_guards)
		end
	end
end

most = 0
the_minute_that_the_sleepiest_guard_slept_the_most_on = -1
the_guard_that_fell_asleep_most_often_on_the_same_minute = -1
max_minutes = 0
do guard = 1 to nr_of_guards
	sleeping = 0
	top = 0
	personal_max = 0
	do minute = 0 to 59
		sleeping = sleeping + guards.guard.minute
		if guards.guard.minute > max_minutes then do
			top = minute
			say 'top >' top
			the_guard_that_fell_asleep_most_often_on_the_same_minute = guard
		end
		personal_max = max(guards.guard.minute, personal_max)
		max_minutes = max(personal_max, max_minutes)
	end
	if sleeping > 0 then say 'Vakt' guard 'sov i' sleeping 'minuter med maxnoteringen' personal_max
	if sleeping > most then do
		say 'new sleepiest with top =' top
		the_minute_that_the_sleepiest_guard_slept_the_most_on = top
		sleepiest = guard
	end
	most = max(sleeping, most)
end
say 'most:' most
say 'top:' top
say 'max_minutes:' max_minutes
say 'sleepiest:' sleepiest
say 'he slept the most on:' the_minute_that_the_sleepiest_guard_slept_the_most_on
say 'nr_of_guards:' nr_of_guards
say 'most often on the same minute:' the_guard_that_fell_asleep_most_often_on_the_same_minute
say sleepiest * the_minute_that_the_sleepiest_guard_slept_the_most_on
say top * the_guard_that_fell_asleep_most_often_on_the_same_minute
say guards.the_guard_that_fell_asleep_most_often_on_the_same_minute.top