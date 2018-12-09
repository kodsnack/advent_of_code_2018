/* Advent of code 2018, day 9, part 1 in ANSI REXX */
players = 491
rounds = 71058

score. = 0

push 0
do round = 1 for rounds
	do 2
		pull #
		queue #
	end
	push round
	if round // 23 = 0 then do
		pull .
		ggr = queued()
		ggr = ggr - 7 - 2
		do ggr
			pull #
			queue #
		end
		pull #
		player = round // players
		score.player = score.player + # + round
	end
end

top = 0
do player = 0 for players
	top = max(top, score.player)
end
say top