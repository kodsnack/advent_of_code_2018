/* Advent of code 2018, day 2, part 2 in ANSI REXX */
fn = 'day2.txt'

do while lines(fn)
	queue linein(fn)
end
call lineout fn

do while queued() > 0
	parse pull id
	do while lines(fn)
		t = linein(fn)
		at = compare(id,t)
		if at = 0 then iterate
		if compare(substr(id, at+1), substr(t, at+1)) = 0 then signal done
	end
	call lineout fn
end
exit

done:
parse var id a =(at) +1 b
say a||b
