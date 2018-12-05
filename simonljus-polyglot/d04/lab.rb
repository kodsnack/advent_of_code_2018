class GuardiansOfTheLab
    def initialize(sleep_mark="s")
        @sleep_mark = sleep_mark
        @purpose = 42
    end
    def log(min,id,asleep,schedule)
        if asleep
            schedule[min]=@sleep_mark
        else
            last_sleep= schedule.reverse.index(@sleep_mark)
            if last_sleep !=nil
                sleep_at= schedule.length - last_sleep - 1
            else
                sleep_at =0
            end
            while sleep_at<min
                schedule[sleep_at] =@sleep_mark
                sleep_at +=1
            end
        end
        return schedule
    end
    def sleepy(guards)
        most_sleep = 0
        most_sleepy =0
        most_sleepy_schedule = Array.new(60){0}
        maxed_minute_count =0
        p2id =0
        maxed_minute_stamp=0
        guards.each_pair{|id,schedules|
            minutes = Array.new(60){0}
            sleeptime =0
            schedules.each{|day|
                day.each_with_index{|status,i|
                    if status ==@sleep_mark
                        minutes[i] +=1
                        sleeptime +=1
                    end
                }
            }    
            if sleeptime > most_sleep
                most_sleepy = id
                most_sleep = sleeptime
                most_sleepy_schedule= minutes
            end
            if minutes.max > maxed_minute_count
                maxed_minute_count= minutes.max
                maxed_minute_stamp =minutes.index(minutes.max)
                p2id = id
            end
        }
        best_minute = most_sleepy_schedule.index(most_sleepy_schedule.max)
        ans1 = most_sleepy.to_i * best_minute 
        ans2 = maxed_minute_stamp * p2id.to_i
        puts "Most sleepy guard: ##{most_sleepy} at minute #{best_minute}"
        puts "Problem 1 checksum: #{ans1}"
        puts "Most sleepy minute: #{maxed_minute_stamp} by guard ##{p2id}"
        puts "Problem 2 checksum: #{ans2}"
    end
end


def drawSchedule(guards)
    guards.keys.sort.each{|id|
        puts "#"+id
        guards[id].each{|schedule| puts schedule.join("")}
    }
end

def solve(filename="input.txt",draw=false,guardError="Error 404: Guard not found")
    solver = GuardiansOfTheLab.new()
    fileObj = File.new(filename, "r")
    id=""
    lines = fileObj.readlines.sort
    guards =Hash.new(guardError)
    schedule=Array.new(60){"."}
    lines.each{|line|
        instr = line.split("]")
        tid = instr[0].split(" ")
        work=instr[1]
        hhmm =tid[1].split(":")
        hour = hhmm[0].to_i
        min =hhmm[1].to_i
        blame=Hash.new("")
        if work.strip == "falls asleep"
            asleep = true
        elsif(work.strip == "wakes up")
            asleep = false
        else 
            id = work.strip.split[1][1..-1]
            schedule = Array.new(60){"."}
            min =0
            if guards[id] == guardError
                guards[id] = [schedule]
            else
                guards[id].push(schedule)
            end
            asleep = false
        end
        schedule = solver.log(min,id,asleep,schedule)
    }
    if draw
        drawSchedule(guards)
    end
    solver.sleepy(guards)
end

if __FILE__ == $0
    solve()
end