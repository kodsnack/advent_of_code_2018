line = File.open('input', &:readline)

data = line.split(' ').map { |c| c.to_i }

class Node
	attr_accessor :metadata, :children, :nc, :nmd

	def initialize(metadata, children) 
		@metadata = metadata
		@children = children
	end

	# part 1
	def sumdata
		return @metadata.sum + @children.map { |c| c.sumdata }.sum
	end

	def part2
		if @children.length == 0 then
			return @metadata.sum
		else
			sum = 0
			@metadata.each do |m|
				node = @children[m-1]
				if node != nil then
					sum += node.part2
				end
			end
			return sum
		end
	end
end



def parse_node(start, data)
	nchildren = data[start]
	ndata = data[start + 1]
	start += 2
	children = []
	
	if nchildren > 0 then
		for i in 1..nchildren do
			r = parse_node(start, data)
			start = r[1]
			children.push(r[0])
		end
	end
	mdata = data[start..(start+ndata-1)]
	start += ndata
	ret = Node.new(mdata, children)

	return [ret, start]
end

top = parse_node(0, data)
puts 'part 1: ' + top[0].sumdata.to_s
puts 'part 2: ' + top[0].part2.to_s

