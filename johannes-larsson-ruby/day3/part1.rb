class Claim
	def initialize(x, y, w, h)
		@x = x
		@y = y
		@w = w
		@h = h
	end
end

f = Hash.new(0)

File.open('input').each do |line|
	parts = line.split(',')
	x = parts[0].split(' ')[-1].to_i
	p2 = parts[1].split(':')
	y = p2[0].to_i
	p3 = p2[1].split('x')
	w = p3[0].to_i
	h = p3[1].to_i

	for dx in 0..w-1
		for dy in 0..h-1
			f[((x + dx).to_s + ',' + (y + dy).to_s)] += 1
		end
	end
end

ans = 0
f.keys.each do |k|
	if (f[k] > 1) then
		ans += 1
	end
end

puts ans

