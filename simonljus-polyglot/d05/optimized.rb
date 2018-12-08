class Something
    def initialize()
        @purpose = 42
        @interval = ("a".bytes[0] -"A".bytes[0]).abs
    end

    def solving(polymer)
        i =1 
        while i < polymer.length
            a= polymer[i-1].bytes[0]
            b= polymer[i].bytes[0]
            if (a-b).abs == 32
                reacted = true
                reaction(polymer,i-1)
                if i !=1
                    i -=1
                end
            else 
            i += 1
            end
        end
        return polymer
    end  
     
    def reaction(polymer,i)
        polymer.delete_at(i)
        polymer.delete_at(i)
    end

end

def solve(filename="input.txt")
    solver = Something.new()
    fileObj = File.new(filename, "r")
    scores= []
    t_start = Time.now
    while (line = fileObj.gets)
        p1=""
        original  = line.strip
        alphabet = original.downcase.split("").uniq.sort
        p1_start = Time.now 
        p1 = solver.solving(original.split(""))
        p1_end = Time.now
        alphabet.each{|letter|
            t_letter_start = Time.now
            filtered =p1.select{|char| char.downcase!=letter}
            aftermath =solver.solving(filtered)
            score = aftermath.length
            puts "letter #{letter} score #{score} time: #{Time.now  - t_letter_start} seconds"
            scores.push(score)
        }
        lowest = scores.min
        p2_end = Time.now
        puts "\nProblem 1 solved: Length #{p1.length}. Took #{p1_end - p1_start} seconds"
        puts "problem 2: letter #{alphabet[scores.index(lowest)]} with length #{lowest}. Took #{p2_end - p1_end} seconds"
    end
    t_end = Time.now
    puts "took #{t_end - t_start} seconds"
end

if __FILE__ == $0
    solve()
end