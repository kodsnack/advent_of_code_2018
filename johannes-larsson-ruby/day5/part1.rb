inp = File.open('input', &:gets).strip!

def process (s)
	i = 0
	while i < s.length - 1
		c1 = s[i]
		c2 = s[i + 1]
		if (((c1 == c1.upcase && c2 == c2.downcase) or (c1 == c1.downcase && c2 == c2.upcase)) and c1.downcase == c2.downcase) then
			if (i > 0) then
				s = s[0..i-1] + s[i+2..s.length]
			else 
				s = s[2..s.length]
			end
		else
			i += 1
		end
	end
	return s
end

def top(inp)
	ninp = inp
	inp = ''
	while ninp != inp do
		inp = ninp
		ninp = process(inp)
	end
	return ninp.length
end

puts 'part 1: ' + top(inp).to_s

cands = []
for c in 'a'..'z' do
	n = inp.gsub(c,'')
	n = n.gsub(c.upcase,'')
	cands.push(n)
end
puts 'part 2: ' + cands.map{ |c| top(c) }.min.to_s
