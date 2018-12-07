class Point
	attr_accessor :x, :y, :a, :edge

	def initialize(x, y)
		@x = x
		@y = y
		@a = 0
		@edge = false
	end
	
	def dist(x, y)
		return (x-@x).abs + (y-@y).abs
	end
end

points = []

File.open('input').each do |line|
	s = line.split(', ')
	points.push(Point.new(s[0].to_i, s[1].to_i))
end

ans2 = 0

minc = -400
maxc = 800

for x in minc..maxc do
	for y in minc..maxc do
		dists = points.map { |p| p.dist(x, y) }
		mind = dists.min
		if (dists.sum < 10000) then
			ans2 += 1
		end
		if (dists.select { |d| d == mind }.length == 1) then
			p = points[dists.find_index(mind)]
			p.a += 1
			if (x == minc or x == maxc or y == minc or y == maxc) then
				for i in 0..points.length-1 do
					if dists[i] == mind then
						points[i].edge = true
					end
				end
			end
		end
	end
end

ne = points.keep_if { |p| not p.edge }
dists = ne.map{ |p| p.a }
puts 'part 1: ' + dists.max.to_s
puts 'part 2: ' + ans2.to_s
