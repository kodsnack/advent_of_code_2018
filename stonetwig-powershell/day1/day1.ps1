$input = Get-Content "day1_input.txt"
$result = 0
foreach ($n in $input) {
    $result = $result + $n
}
"Part 1: " + $result

$reached = New-Object System.Collections.Generic.HashSet[int]
$twice = -1
$result = 0
while ($twice -eq -1) {
    foreach ($n in $input) {
        $result = $result + $n
        $couldBeAdded = $reached.Add($result)
        if (-Not $couldBeAdded) {
            $twice = $result
            break
        }
    }
}
"Part 2: " + $twice