require 'set'
deps = {}
File.open('input').each do |line|
	s = line.split(' ')
	dependency = s[1]
	dependant = s[7]
	if (not deps.key? dependant) then
	       deps[dependant] = []
	end
	deps[dependant].push(dependency)
end

iset = Set.new
deps.keys.each do |k|
	deps[k].each do |c|
		iset.add c
	end
	iset.add k
end

num_ins = iset.length

done = Set.new

def can_do(ins, deps, done)
	d = deps[ins]
	if d == nil then
		d = []
	end
	d.each do |i|
		if (not done.include? i) then
			return false
		end
	end
	return true
end

working = {}

times = {}
i = 60;
('A'..'Z').each { |c| 
	i += 1
	times[c] = i
}

doing = Set.new
time = 0

while done.length < num_ins do
	av = iset.select { |i| can_do(i, deps, done) }
	av.sort!
	if av.length < 1 then

	else
		av.each do |a|
			doing.add a
			iset.delete a
		end
	end

	print 'doing: '
	doing.each do |c|
		print c
		times[c] -= 1
		if times[c] == 0 then
			done.add c
			doing.delete c
		end
	end
	time += 1
	puts
end

puts time
