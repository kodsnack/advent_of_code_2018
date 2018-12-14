require 'set'

sum = 0
oldsums = Set[]

while true do
	File.open('input').each do |line|
		if oldsums === sum
			puts sum
			exit
		else
			oldsums.add(sum)
			sum += line.to_i
		end
	end
end
