<?php

namespace src\AdventOfCode;

class Day5 extends AbstractAdventOfCode
{
    protected $class = 5;

    public function firstPart(): string
    {
        return strlen($this->fullyReacting($this->input));
    }

    public function secondPart(): string
    {
        $input = $this->input;
        $shortestPolymer = PHP_INT_MAX;
        foreach (range('a', 'z') as $char) {
            $tmpInput = str_ireplace($char, '', $input);
            $tmp = strlen($this->fullyReacting($tmpInput));
            $shortestPolymer = min($shortestPolymer, $tmp);
        }

        return $shortestPolymer;
    }

    private function fullyReacting(string $input): string
    {
        $i = 1;
        while ($i < strlen($input)) {
            if (ctype_lower($input[$i-1]) && ctype_upper($input[$i]) && strcasecmp($input[$i-1], $input[$i]) === 0) {
                $input = substr($input, 0, $i-1) . substr($input, $i+1);
                $i = 0;
            } elseif (ctype_upper($input[$i-1]) && ctype_lower($input[$i]) && strcasecmp($input[$i-1], $input[$i]) === 0) {
                $input = substr($input, 0, $i-1) . substr($input, $i+1);
                $i = 0;
            }
            $i++;
        }

        return $input;
    }
}
