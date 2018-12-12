i = 0
rules = {}
state = ''
npad = 40
File.open('input').each do |line|
	if (i == 0) then
		padding = '.'*npad
		state = padding + line.split(' ')[2] + padding
	elsif (i > 1) then
		s = line.split('=>')
		rules[s[0].strip] = s[1].strip
	end
	i += 1
end


puts state

puts rules.length.to_s + ' rules'

rules.keys.each do |k|
	puts k + ': ' + rules[k]
end



def filter(data, rules, pos)
	d = (".."+data+"..")[pos..pos+4]
	r = rules[d]
	return r
#	if r == nil then
#		puts 'nil'
#	else 
#		puts r
#	end
end

for i in 1..20 do
	nstate = ''
	puts state
	for p in 0..(state.length-1) do
		nstate += filter(state, rules, p)
	end
	state = nstate
end

sum = 0
state.chars.each_with_index { |c,i| 
	if c == '#' then
		sum += (i - npad)
	end
}
puts sum
