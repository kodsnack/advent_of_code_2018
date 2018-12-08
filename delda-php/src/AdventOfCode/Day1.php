<?php

namespace src\AdventOfCode;

class Day1 extends AbstractAdventOfCode
{
    protected $class = 1;

    public function firstPart(): string
    {
        return $this->adder(0);
    }

    public function secondPart(): string
    {
        return $this->twice(0);
    }

    private function adder(int $frequency): int
    {
        $input = explode(PHP_EOL, $this->input);
        foreach ($input as $change) {
            preg_match('/([+-])(\d+)/', $change, $matches);

            if ($matches[1] === '+') {
                $frequency += $matches[2];
            } else {
                $frequency -= $matches[2];
            }
        }

        return $frequency;
    }

    private function twice($frequency): int
    {
        $input = explode(PHP_EOL, $this->input);
        $sizeOfInput = sizeof($input);
        $counter = 0;
        $duplicate = [];

        while ($counter < $sizeOfInput) {
            preg_match('/([+-])(\d+)/', $input[$counter], $matches);

            if ($matches[1] === '+') {
                $frequency += $matches[2];
            } else {
                $frequency -= $matches[2];
            }

            if (isset($duplicate[$frequency])) {
                return $frequency;
            } else {
                $duplicate[$frequency] = true;
            }

            $duplicate[$frequency] = true;

            $counter++;
            if ($counter == $sizeOfInput) {
                $counter = 0;
            }
        }
    }
}
