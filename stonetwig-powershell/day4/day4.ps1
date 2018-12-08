$input = Get-Content "input.txt" | Sort-Object
$guards = @{}
$shifts = @{}

$lastGuard = 0
$minuteAsleep = -1

foreach ($string in $input) {
    [System.Collections.ArrayList]$parts = $string.Split(' ')
    $date = ($parts[0] + " " + $parts[1]).Replace('[', '').Replace(']', '')
    $date = Get-Date -Date $date
    $parts.Remove($parts[0])
    $parts.Remove($parts[0])
    $rest = $parts -Join ' '
    
    if ($rest -like "*#*") {
       $guardId = $rest -replace '\D+(\d+)*','$1'
       $lastGuard = $guardId
       if ($guards.ContainsKey($guardId)) {
           $shifts[$guardId] = $shifts[$guardId] += 1
       } else {
           $guards.Add($guardId, @())
           $shifts.Add($guardId, 1)
       }
    } elseif ($rest -like "falls asleep") {
        $minuteAsleep = $date.Minute
    } elseif ($rest -like "wakes up") {
        for ($i = $minuteAsleep; $i -lt $date.Minute; $i++) {
            $guards[$lastGuard] = $guards[$lastGuard] += $i
        }
    }
}

$sleepyGuard = 0
$minutesAsleep = 0

foreach ($h in $guards.GetEnumerator()) {
    $map = ($h.Value | group | sort count -desc | select -f 1)
    if ($h.Value.Count -gt $minutesAsleep) {
        $minutesAsleep = $h.Value.Count
        $frequentMinuteCount = $map.Count
        $sleepyGuard = $h.Name
        $minute = $map.Name
    }
}

"The most sleepy guard is #$sleepyGuard at minute $minute, with $minutesAsleep minutes asleep and he slept at that minute $frequentMinuteCount times"
"Day 4, part 1 answer is " + ([convert]::ToInt32($sleepyGuard) * [convert]::ToInt32($minute))

$frequentMinuteCount = 0
$sleepyGuard = 0
$minutesAsleep = 0

foreach ($h in $guards.GetEnumerator()) {
    $map = ($h.Value | group | sort count -desc | select -f 1)
    if ($map.Count -gt $frequentMinuteCount) {
        $minutesAsleep = $h.Value.Count
        $frequentMinuteCount = $map.Count
        $sleepyGuard = $h.Name
        $minute = $map.Name
    }
}

"The most sleepy guard is #$sleepyGuard at minute $minute, with $minutesAsleep minutes asleep and he slept at that minute $frequentMinuteCount times"
"Day 4, part 2 answer is " + ([convert]::ToInt32($sleepyGuard) * [convert]::ToInt32($minute))