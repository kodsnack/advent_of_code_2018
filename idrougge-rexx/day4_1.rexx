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
				say i
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

/*
most = 0
minute = -1
do guard = 1 to 100
	do i = 0 to 59
		if guards.guard.i > most then minute = i
		most = max(guards.guard.i, most)
	end
end
say 'most:' most
say 'minute:' minute
*/

most = 0
minute = -1
do guard = 1 to nr_of_guards
	/*
	say 'Vakt' guard
	*/
	sleeping = 0
	top = 0
	max_minutes = 0
	do i = 0 to 59
		sleeping = sleeping + guards.guard.i
		if guards.guard.i > max_minutes then top = i
		max_minutes = max(guards.guard.i, max_minutes)
		/*
		say i':' guards.guard.i '>' sleeping
		*/
	end
	if sleeping > 0 then say 'Vakt' guard 'sov i' sleeping 'minuter'
	if sleeping > most then do
		minute = top
		sleepiest = guard
	end
	most = max(sleeping, most)
end
say 'most:' most
say 'top:' top
say 'max_minutes:' max_minutes
say 'minute:' minute
say 'sleepiest:' sleepiest
say 'nr_of_guards:' nr_of_guards
say sleepiest * minute