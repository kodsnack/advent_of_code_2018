$input = Get-Content "day3_input.txt"
$overlaps = 0
$square = 1..1500 | ForEach-Object { ,(,2 * 1500) }
foreach ($line in $input) {
    $parts = $line.Split(" ")
    $hash = $parts[0]
    $pos = $parts[2].Replace(":", "").Split(",")
    $xy = $parts[3].Split("x")

    $ilimit = [convert]::ToInt32($pos[1]) + [convert]::ToInt32($xy[1])
    $jlimit = [convert]::ToInt32($pos[0]) + [convert]::ToInt32($xy[0])

    for ($i = [convert]::ToInt32($pos[1]); $i -lt $ilimit; $i++) {
        for ($j = [convert]::ToInt32($pos[0]); $j -lt $jlimit; $j++) {
            if ($square[$i][$j].ToString().StartsWith("#")) {
                $square[$i][$j] = "X"
                $overlaps++
            } elseif ($square[$i][$j] -ne "X") {
                $square[$i][$j] = $hash
            }
        }
    }
}

"Day 3, part 1, number of overlaps: " + $overlaps

foreach ($line in $input) {
    $parts = $line.Split(" ")
    $hash = $parts[0]
    $pos = $parts[2].Replace(":", "").Split(",")
    $xy = $parts[3].Split("x")

    $ilimit = [convert]::ToInt32($pos[1]) + [convert]::ToInt32($xy[1])
    $jlimit = [convert]::ToInt32($pos[0]) + [convert]::ToInt32($xy[0])
    $intact = 1

    for ($i = [convert]::ToInt32($pos[1]); $i -lt $ilimit; $i++) {
        for ($j = [convert]::ToInt32($pos[0]); $j -lt $jlimit; $j++) {
            if ($square[$i][$j] -eq "X") {
                $intact = 0
            }
        }
    }

    if ($intact) {
        "Day 3, part 2, claim that is intact " + $hash
        break 1
    }
}