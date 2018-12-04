class Claim
	attr :x, :y, :w, :h, :id
	def initialize(id, x, y, w, h)
		@x = x
		@y = y
		@w = w
		@h = h
		@id = id
	end
end

f = Hash.new(0)
claims = []

File.open('input').each do |line|
	parts = line.split(',')
	p1 = parts[0].split(' ')
	id = p1[0][1..p1[0].length]
	x = p1[-1].to_i
	p2 = parts[1].split(':')
	y = p2[0].to_i
	p3 = p2[1].split('x')
	w = p3[0].to_i
	h = p3[1].to_i
	claims.push(Claim.new(id, x, y, w, h))

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

puts 'part 1: ' + ans.to_s

claims.each do |c|
	good = true
	for dx in 0..(c.w - 1)
		for dy in 0..(c.h - 1)
			if (f[((c.x + dx).to_s + ',' + (c.y + dy).to_s)] != 1) then
				good = false
			end
		end
	end
	if (good) then
		puts 'part 2: ' + c.id
	end
end

