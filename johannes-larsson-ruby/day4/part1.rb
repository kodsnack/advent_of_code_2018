events = []
File.open('input').each do |line|
	events.push(line)
end

events.sort_by! { |l| l[0..16] }

guards = {}
start = -1
cg = ''

for i in 0..events.length-1 do
	p = events[i].split(' ')		
	if (p[2] == 'Guard') then

		cg = p[3][1..100]
		if (not guards.key? cg) then
			guards[cg] = [0] * 60
		end
	elsif (p[2] == 'wakes') then
		e = p[1].split(':')[1][0..1].to_i
		for j in start..e-1 do
			guards[cg][j] += 1
		end
		start = -1
	elsif (p[2] == 'falls') then
		start = p[1].split(':')[1][0..1].to_i
	end
end

gindex = guards.keys.max_by { |k| guards[k].sum  }
min =  (0..59).max_by { |i| guards[gindex][i] }
puts 'part 1: ' + (min * gindex.to_i).to_s

gindex = guards.keys.max_by { |k| guards[k].max }
min =  (0..59).max_by { |i| guards[gindex][i] }
puts 'part 2: ' + (min*gindex.to_i).to_s
