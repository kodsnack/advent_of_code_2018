$input = Get-Content "day2_input.txt"
$twice = 0
$thrice = 0

foreach ($string in $input) {
    $hit = New-Object System.Collections.Generic.HashSet[char]
    $counted = "none"
    foreach ($char in $string.ToCharArray()) {
        $occurance = ($string.ToCharArray() | Where-Object {$_ -eq $char} | Measure-Object).Count
        if (($occurance -eq 2) -and ($counted -ne "twice")) {
            if ($hit.Add($char)) {
                $twice += 1
                $counted = "twice"
            }
        }
        if (($occurance -eq 3) -and ($counted -ne "thrice")) {
            if ($hit.Add($char)) {
                $thrice += 1
                $counted = "thrice"
            }
        }
    }
}

"Day 1, part 1: $twice * $thrice = " + ($twice * $thrice) + " is the answer"
function Get-Diff {
    param(
        [String] $string1,
        [String] $string2
    )
    if ( $string1 -ceq $string2 ) {
        return -1
    }
    $count = 0
    for ( $i = 0; $i -lt $string1.Length; $i++ ) {
        if ( $string1[$i] -cne $string2[$i] ) {
            $count++
        }
    }
    return $count
}
foreach ($x in $input) {
    foreach ($y in $input) {
        $diff = Get-Diff $x $y
        if ($diff -eq 1) {
            $diff = Compare-Object $x.ToCharArray() $y.ToCharArray() -IncludeEqual -ExcludeDifferent -PassThru
            $left = $diff -Join ''
            "Day 1, part 2:  $left"
            break 1
        }
    }
}