def compareWords(word1, word2)
	i = 0
	index = -1
	until i >= word1.length do
		if (word1[i] != word2[i]) then
			if (index != -1) then
				return false
			else
				index = i
			end
		end
		i += 1
	end
	
	return word1[0..index-1] + word1[index+1..word1.length-1]
end


words = []

File.open('input').each do |line|
	words.each { |word|
		res = compareWords(word,line)
		if (res != false)
			puts res
			exit
		end
	}
	words.push(line)
end
