class Node
    attr_accessor :value
    attr_accessor :children
    attr_accessor :metadata
    attr_accessor :sum
    def initialize()
        @children=[]
        @metadata=[]
        @value =0
        @sum =0
    end
    def fill(line)
        if line.length >=2
            n_children,n_metadata= line.shift(2)
            n_children.times{
                child=Node.new()
                child.fill(line)
                @sum +=child.sum
                @children.push(child)
            }
            metadata=line.shift(n_metadata)
            @metadata=metadata
            metadata_sum=  metadata.inject{|sum,x| sum +x}
            @sum +=metadata_sum
            if n_children ==0
                @value = metadata_sum
            else
                metadata.each{|child_index|
                    if @children.length >= child_index
                        @value+= @children[child_index -1].value 
                    end
                } 
            end
            return @sum,@value
        end
    end
end
def solve(filename="input.txt")
    fileObj = File.new(filename,"r")
    line = fileObj.readline.split(" ").map{|data| data.to_i}
    root = Node.new()
    p1,p2=root.fill(line)   
    return [p1,p2]
end
if __FILE__ == $0
    p1ans =36566 
    p2ans= 30548
    testcase1,testcase2 = solve("testcase.txt")
    raise "test case failed #{testcase1}" if testcase1 != 138
    puts "Testcase 1 passed!"
    p1,p2 = solve("input.txt")
    #raise "Problem1 failed #{p1}" if p1 != p1ans 
    puts "Problem 1 solved, the sum is: #{p1}"
    raise "test case 2 failed #{testcase2}" if testcase2 != 66
    puts "Testcase 2 passed!"
    #raise "Problem1 failed #{p1}" if p2 != p2ans
    puts "Problem 2 solved, the value is: #{p2}"
end