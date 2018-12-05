$input = Get-Content "input.txt"
$chars = New-Object System.Collections.Generic.HashSet[string]

function Get-Redacted {
    param(
        [string] $string,
        [Parameter(Mandatory=$False)]
        [string] $extra = ""
    )
    for ($i = 0; $i -lt ($string.Length - 1); $i++) {
        $added = $chars.Add($string[$i])
        if (($string[$i] -eq $string[$i + 1]) -and ((($extra -eq "") -and ($string[$i] -cne $string[$i + 1])) -or ($extra -eq $string[$i + 1]))) {
            $string = $string.Remove($i, 2)
            $j = $i - 3
            if ($j -gt 0) {
                $i = $j
            } else {
                $i = 0
            }
        }
    }
    if (($string[0] -eq $string[1]) -and ($string[0] -cne $string[1])) {
        $string = $string.Remove(0, 2)
    }
    return $string
}
$redacted = Get-Redacted $input
"Day5, part 1: " + $redacted.Length

$lowestChar = ""
$shortestRedact = $input

foreach ($c in $chars) {
    $redacted = $input -replace "$c"
    $redacted = Get-Redacted $redacted
    if ($redacted.Length -lt $shortestRedact.Length) {
        $lowestChar = $c
        $shortestRedact = $redacted
    }
}

"Day5, part 2: " + $shortestRedact.Length

