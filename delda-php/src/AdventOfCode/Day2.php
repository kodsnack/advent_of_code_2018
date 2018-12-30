<?php

namespace src\AdventOfCode;

class Day2 extends AbstractAdventOfCode
{
    protected $class = 2;

    public function firstPart(): string
    {
        return $this->checksum();
    }

    public function secondPart(): string
    {
        return $this->commonLetters();
    }

    private function checksum(): int
    {
        $ids = explode(PHP_EOL, $this->input);
        $twoTimes = 0;
        $threeTimes = 0;

        foreach ($ids as $id) {
            $count = array_count_values(str_split($id));
            if (in_array(2, $count)) {
                $twoTimes++;
            }
            if (in_array(3, $count)) {
                $threeTimes++;
            }
        }

        return $twoTimes * $threeTimes;
    }

    private function commonLetters(): string
    {
        $input = explode(PHP_EOL, $this->input);

        $result = '';
        $sizeOfInput = sizeof($input);

        for ($i = 0; $i < $sizeOfInput; $i++) {
            for ($j = $i + 1; $j < $sizeOfInput; $j++) {
                $result .= $this->diff($input[$i], $input[$j]);
            }
        }

        return $result;
    }

    private function diff(string $stringA, string  $stringB): string
    {
        $differences = 0;
        $result = '';

        for ($i = 0; $i < strlen($stringA); $i++) {
            if ($stringA[$i] == $stringB[$i]) {
                $result .= $stringA[$i];
            } else {
                $differences++;
            }
        }

        if ($differences == 1) {
            return $result;
        } else {
            return '';
        }
    }
}
