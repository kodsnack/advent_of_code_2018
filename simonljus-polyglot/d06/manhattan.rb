
class Coordinate
    attr_accessor :rowval
    attr_accessor :colval
    attr_accessor :logo
    def initialize(row,col,logo="woop")
        @colval=col.to_i
        @rowval=row.to_i 
        @logo = logo.to_s
        @infinite = false
        @score =0
    end
    def manhattan(row,col)
        distance = ( col - @colval ).abs + ( row - @rowval ).abs
        return distance
    end
    def to_s()
        return " row:#{@rowval} col:#{@colval} mark:#{@logo}"
    end
    def getscore()
        if @infinite
            return -1
        else
            return @score
        end
    end
    def increaseScore(isInfinite)
        @score +=1
        if @infinite || isInfinite
            @infinite = true 
        end
    end

end
def build 
end
def adjust(coords,row_min,col_min) 
    coords.each{|coord|
    coord.adjust(row_min,col_min)
    }
end
def drawGrid(coords,row_range,col_range,limit=10000) 
    row_extra =1
    col_extra=1
    row_max = row_extra + row_range.end
    col_max = col_extra + col_range.end
    grid   = Array.new(row_range.end + row_extra){Array.new(col_range.end + col_extra){"."}}
    i =0
    coords.each{|coord|
        raise "too large row #{row_max} vs #{coord.rowval}" if coord.rowval > row_max
        raise "too large col #{col_max} vs #{coord.colval}" if coord.colval > col_max
        raise "too small row #{row_min} vs #{coord.rowval}" if coord.rowval < 0
        raise "too small col #{col_min} vs #{coord.colval}" if coord.colval < 0
        grid[coord.rowval][coord.colval] = i.to_s
        i +=1
    }
    r_i=0
    p1_grid = []
    center_coordinate_count=0
    grid.each{|rowvalues|
        walk_row_start = Time.now
        c_i=0
        p1_row= []
        is_hit_row = r_i <= row_range.begin || r_i >= row_range.end
        rowvalues.each{|cell|
            
            is_hit_col = c_i <= col_range.begin || c_i >= row_range.end
            is_hit_edge = is_hit_row || is_hit_col
            min_val = 10000
            marked ="."
            closest_coord = nil
            tot_distance =0
            coords.each{|coordelement|
                distance = coordelement.manhattan(r_i,c_i)
                if distance <= min_val
                    if distance == min_val 
                        marked ="x"
                    else
                        closest_coord = coordelement  
                        marked = coordelement.logo
                        min_val = distance
                    end
                end
                tot_distance += distance
            }
            if !is_hit_edge && tot_distance< limit
                center_coordinate_count+=1
            end
            raise "none was the closest " if  closest_coord == nil && marked !="x"
            closest_coord.increaseScore(is_hit_edge)  
            c_i +=1  
            is_hit_edge = false  
            p1_row.push(marked.to_s)
        }

        walk_row_end =Time.now 
        p1_grid.push(p1_row)
        r_i +=1
    }  
    #p1_grid.each{|row|
    #    puts row.join(",")
    #}
    best_coord_score =0
    best_coord = nil
    coords.each{|coordop|
        if(coordop.getscore() >best_coord_score )
            best_coord=coordop
            best_coord_score = coordop.getscore()
        end
    }
    
    #puts "Problem 1 solved: Coordinate #{best_coord} was the best with area #{best_coord.getscore()} "
    #puts "Problem 2 solved: Center coordinate count  #{center_coordinate_count} "
    return [best_coord.getscore(),center_coordinate_count]
end

def solve(filename="input.txt",limit=32)
    grid = Hash.new("coordnotfound")
    fileObj = File.new(filename, "r")
    scores= []
    coordinates =[]
    t_start = Time.now
    col_min =1000
    row_min =1000
    col_vals =[]
    row_vals =[]
    i=0
    while (line = fileObj.gets)
        colstr,rowstr =line.split(",")
        col= colstr.strip.to_i
        row= rowstr.strip.to_i
        coord = Coordinate.new(row,col,i)
        coordinates.push(coord)
        col_vals.push(col)
        row_vals.push(row)
        i+=1
    end
    col_min = col_vals.min
    col_max = col_vals.max
    row_min = row_vals.min
    row_max = row_vals.max
    row_range = (row_min..row_max)
    col_range = (col_min..col_max)
    return drawGrid(coordinates,row_range,col_range,limit=limit)
end

if __FILE__ == $0
    p1ans=5187
    p2ans=34829
    problem_start = Time.now    
    testcase1,testcase2= solve("testcase.txt",32)
    raise "testcase1 failed #{testcase1}" if testcase1 !=17
    raise "testcase2 failed #{testcase2}" if testcase2 !=16
    p1,p2= solve("input.txt",limit=10000)
    problem_end = Time.now
    raise "problem1 failed #{p1}" if p1 !=p1ans
    puts "problem 1 solved, The largest finite area is  #{p1} "
    raise "problem 2 failed #{p2}" if p2 !=p2ans
    puts "problem 2 solved, Center coordinate count  #{p2} "
    puts " It took #{problem_end - problem_start} seconds to solve the problem"
   
end
