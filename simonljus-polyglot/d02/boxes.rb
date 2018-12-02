
class Checker
    attr_accessor :common
    def initialize()
        @pairs=0;
        @threes=0;
        @ids =[]
        @common=""
    end
    def check(id)
        prevChar ='';
        count = 0;
        hasPairs = false;
        hasThrees =false;
        id+= "|"
        arr = id.chars.sort
        arr.each() do |c|   
            if c == prevChar
                count +=1;
            else
                if count == 1
                    hasPairs = true
                elsif count == 2
                    hasThrees = true
                end
                prevChar = c
                count = 0
            end
            
        end
        if hasPairs == true
            @pairs += 1
        end
        if hasThrees == true
            @threes += 1
        end
    end
    def getChecksum()
        return @pairs * @threes
    end
    def closeMatch(newId)
        nId = newId.strip.chars
        @ids.each() do |id|
            if id.zip(nId).count{|a,b| a !=b} == 1
                @common = findCommon(id,nId)
                return true
            end
        end
        @ids.push(nId)
        return false   
    end
    def findCommon(arrA,arrB)
        return arrA.zip(arrB).map{|a,b| a ==b ? a : ""}.join("")
    end
end

def solve(checker)
    fileObj = File.new("input.txt", "r")
    p2Solved = false
    while line = fileObj.gets
            checker.check(line)
            if p2Solved == false && checker.closeMatch(line) == true
                p2Solved = true
            end
    end
    puts "Problem 1 solved: The checksun is: #{checker.getChecksum()} \n";
    puts "Problem 2 solved: The common characters are: #{checker.common} \n";
end

if __FILE__ == $0
    checker = Checker.new()
    solve(checker)
end
