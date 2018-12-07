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

while done.length < num_ins do
	av = iset.select { |i| can_do(i, deps, done) }
	av.sort!
	if av.length < 1 then
		puts 'cant do anything'
		exit
	else
		done.add av[0]
		iset.delete av[0]
		print av[0]
	end
end


