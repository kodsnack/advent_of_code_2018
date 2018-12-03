require 'Set'
class Solver
    attr_accessor :area
    attr_accessor :fine
    def initialize(fabric,unclaimed)
        @area = 0
        @fabric=fabric
        @fine=Set.new()
        @overlap ="x"
        @unclaimed =unclaimed
    end
    def addClaims(lines)
        lines.each{|line|
            instr= line.split("@")
            id = instr[0].strip
            fine.add(id)
            coords = instr[1].split(":")
            pos = coords[0].split(",")
            lols = coords[1].split("x")
            h = lols[1].strip.to_i
            w = lols[0].strip.to_i
            x = pos[0].strip.to_i
            y = pos[1].strip.to_i
            hind =0
            while(hind <h)
                wind = 0
                while (wind < w)
                    mark= @fabric[y+hind][x+wind]
                    if  mark ==@unclaimed
                        @fabric[y+hind][x+wind]=id
                    else
                        fine.delete(id)
                        fine.delete(mark)
                        @fabric[y+hind][x+wind] =@overlap
                    end
                    wind +=1
                end
                hind += 1
            end
        }
    end
    def countMatches()
        @fabric.each{ |row|
            row.each{ |cell|
                if cell == @overlap
                    @area += 1
                end
            }

        }
    end
end
def solve()
    unclaimed ="."
    fabric =[]
    i = 1000
    while i >0
        j=1000
        row =[]
        while (j>0)
            row.push(unclaimed)
            j -= 1
        end
        fabric.push(row)
        i -=1
    end
    solver = Solver.new(fabric,unclaimed)
    fileObj = File.new("input.txt", "r")
    lines =[]
    while(line = fileObj.gets)
        lines.push(line)
    end
    solver.addClaims(lines)
    solver.countMatches()
    puts "Problem 1 solved: #{solver.area} square inches are claimed"
    puts "Problem 2 solved: #{solver.fine.to_a.join("")} is not overlapped"
    
end
if __FILE__ == $0
    solve()
end
