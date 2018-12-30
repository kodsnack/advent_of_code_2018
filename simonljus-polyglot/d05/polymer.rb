class Polymer
    def initialize()
        @purpose = 42
        @interval = ("a".bytes[0] -"A".bytes[0]).abs
    end
    def solving(polymer)
        reacted = true 
        while reacted
            reacted = false
            i =1 
            while i < polymer.length
                a= polymer[i-1].bytes[0]
                b= polymer[i].bytes[0]
                if (a - b).abs == @interval
                    reacted = true
                    polymer =reaction(polymer,i)
                    if(i != 1)
                        i -=1
                    end
                else 
                    i += 1
                end
            end
        end
        return polymer
    end
        
    def reaction(polymer,i)
        str=polymer
        str.each_char{|f_val| raise " str is wrong" if f_val ==nil}
        if i==1
            return str[2..-1]
        elsif i +1 ==polymer.length
            return str[0..-3]
        else 
            first_start= [i-2,0].min
            first_end = i-2
            first = str[first_start..first_end]
            second_start = i+1
            second_end = -1
            second = str[second_start..second_end]
            n_string = first + second
            raise "too long after concat #{n_string.length} vs #{polymer.length} "if ((n_string.length) +2) > polymer.length
            return n_string
        end
    end

end




def solve(filename="input.txt")
    solver = Polymer.new()
    fileObj = File.new(filename, "r")
    scores= []
    t_start = Time.now
    while (line = fileObj.gets)
        original  = line.strip
        alphabet = original.downcase.split("").uniq.sort
        p1_start = Time.now
        p1= solver.solving(original.split("").join(""))
        p1_end = Time.now
        alphabet.each{|letter|
            t_letter_start = Time.now
            filtered =original.split("").select{|char| char.downcase!=letter}
            aftermath =solver.solving(filtered.join(""))
            score = aftermath.length
            puts "letter #{letter}, score #{score}, time: #{Time.now  - t_letter_start} seconds"
            scores.push(score)
        }
        lowest = scores.min
        p1_end = Time.now
        puts "\nProblem 1 solved: Length #{p1.length}. Time: #{p1_end - p1_start} seconds"
        puts "Problem 2 solved: letter #{alphabet[scores.index(lowest)]} with length #{lowest}. Took #{p2_end - p1_end} seconds"
    end
    t_end = Time.now
    puts "took #{t_end - t_start} seconds"
end

if __FILE__ == $0
    solve()
end