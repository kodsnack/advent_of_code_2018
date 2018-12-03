sum = 0

File.open('input').each do |line|
	sum += line.to_i
end

print sum
