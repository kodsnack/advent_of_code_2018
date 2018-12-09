require 'Set'

class Worker
    attr_accessor :time
    def initialize(delay =nil)
    @letter=""
    @time=0
    @delay =delay
    @extra = "A".each_byte.to_a[0]
    end
    def work(timestep)
        if @delay == nil
            temp = @letter
            @letter=""
            return temp
        end
        if @letter.length >0
            @time -=timestep
            if @time <=0 
                raise "time is less than zero" if @time < 0
                temp = @letter
                @letter=""
                return temp
            else
                return ""
            end
        else 
            return ""
        end
    end
    def giveWork(letter)
        if letter.length >0 && @letter.length ==0
            if @delay != nil
                @time = @delay + (@extra  - letter.each_byte.to_a[0]).abs + 1
            else
                @time=0
            end
            @letter =letter
            return true
        else
            return false 
        end
    end

end

def solve(filename="input.txt",n_workers =5,delay=60,timestep=1)
    workers =Array.new(n_workers){Worker.new(delay)}
    fileobj = File.new(filename,"r")
    letters=Set.new()
    special=""
    links = Hash.new {|h,k| h[k] = [] }
    while (line = fileobj.gets)
        words = line.strip().split(" ")
        first = words[1]
        second = words[7]
        letters.add(first)
        letters.add(second)
        links[second].push(first)
    end
    letters= letters.to_a.sort
    final_length =letters.length
    time =0
    min_time =1
    iterations =0
    while special.length < final_length
        workers.each{|worker|
            best_letter = getBest(letters,links)
            if worker.giveWork(best_letter)
                letters.delete(best_letter)
                links.delete(best_letter)
            end
        }
        workers.each{|worker|
            worked = worker.work(timestep)
            special+=worked
            removeDependencies(links,worked)
        }
        time +=timestep   
    end
    return [special,time] 
end

def removeDependencies(links,letter)
    if letter.length >0
        links.each{|k,dependencies|
            dependencies.delete(letter)
        }
    end
end


def getBest(letters,links)
    min =nil
    best_letter = nil
    letters.each{|letter|
        nlinks = links[letter].length
        if nlinks ==0 && (best_letter == nil || letter < best_letter) 
            min = nlinks
            best_letter = letter
        end
    }
    if min ==0
        return best_letter
    else
        return ""
    end
end

if __FILE__ == $0
    p1ans="DFOQPTELAYRVUMXHKWSGZBCJIN"
    p2ans=1036
    testcase1,_ = solve(filename="testcase.txt",n_workers =1,delay=nil,timestep=0)
    raise "test case failed #{testcase1}" if testcase1 != "CABDFE"
    puts "Testcase 1 passed"
    testcase2,timetest = solve(filename="testcase.txt",n_workers =2,delay=0,timestep=1)
    raise "test case failed #{testcase2}, #{timetest} timesteps" if testcase2 != "CABDFE" && timetest !=15
    puts "Testcase 2 passed"
    p1,_ = solve(filename="input.txt",n_workers =1,delay=nil,timestep=0)
    raise "Par1 failed #{p1} " if p1ans != p1
    puts "Part1 solved: #{p1}"
    p1,p2 = solve(filename="input.txt",n_workers =5,delay=60,timestep=1)
    raise "Par2 failed #{p1} #{p2} " if p1ans !=p1 && p2ans != p2
    puts "Part2 solved: #{p1} took #{p2} timesteps"
    min_workers=0
    nw=1
    min_time =nil
    loop do
        p3,time3 = solve(filename="input.txt",n_workers =nw,delay=60,timestep=1)
        if min_time ==nil  || time3 < min_time
            min_time = time3
            min_workers =nw
        else
            break
        end
        nw+=1
    end
    puts "Only requires #{min_workers} workers to optimize the time"
end