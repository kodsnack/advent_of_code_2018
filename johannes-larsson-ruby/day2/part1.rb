doubles = 0
triples = 0

File.open('input').each do |line|
	letters = Hash.new(0)
	line.chars.each do |char|
		letters[char] += 1
	end
	hasdouble = false
	hastriple = false
	letters.keys.each do |key|
		if (letters[key] == 2 and not hasdouble)
			doubles += 1
			hasdouble = true
		elsif (letters[key] == 3 and not hastriple)
			triples += 1
			hastriple = true
		end
	end
end

puts doubles * triples
