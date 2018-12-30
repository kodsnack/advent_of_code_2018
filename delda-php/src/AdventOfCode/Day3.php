<?php

namespace src\AdventOfCode;

use phpDocumentor\Reflection\Types\Nullable;

class Day3 extends AbstractAdventOfCode
{
    const fabricDim = 1000;

    protected $class = 3;

    private $fabric = [];
    private $fabricDimention;

    public function firstPart(int $fabricDimention = 0): string
    {
        $this->squareInches($fabricDimention);

        return $this->sumClaims();
    }

    public function secondPart(int $fabricDimention = 0): string
    {
        $this->squareInches($fabricDimention);

        $inputs = explode(PHP_EOL, $this->input);
        foreach ($inputs as $input) {
            $claim = $this->parse($input);
            if (!$this->checkClaims($claim)) {
                return $claim[1];
            }
        }
    }

    private function parse(string $input) : array
    {
        preg_match('/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/', $input, $matches);

        return $matches;
    }

    private function squareInches(int $fabricDimention)
    {
        $this->fabricDimention = ($fabricDimention == 0) ? self::fabricDim : $fabricDimention;
        $this->fabric = array_fill(0, $this->fabricDimention * $this->fabricDimention, 0);
        $inputs = explode(PHP_EOL, $this->input);
        foreach ($inputs as $input) {
            $this->fillFabric($this->parse($input));
        }
    }

    private function fillFabric(array $square)
    {
        for ($y = 0; $y < $square[5]; $y++) {
            for ($x = 0; $x < $square[4]; $x++) {
                $this->fabric[($this->fabricDimention * ($y+$square[3])) + ($x+$square[2])] += 1;
            }
        }
    }

    private function sumClaims() : int
    {
        return sizeof(array_filter($this->fabric, function ($value) {
            return $value > 1;
        }));
    }

    private function checkClaims(array $square) : bool
    {
        for ($y = 0; $y < $square[5]; $y++) {
            for ($x = 0; $x < $square[4]; $x++) {
                if ($this->fabric[($this->fabricDimention * ($y+$square[3])) + ($x+$square[2])] > 1) {
                    return true;
                }
            }
        }

        return false;
    }

    private function printFabric()
    {
        echo PHP_EOL;
        for ($y = 0; $y < $this->fabricDimention; $y++) {
            for ($x = 0; $x < $this->fabricDimention; $x++) {
                echo $this->fabric[($this->fabricDimention * $y) + $x];
            }
            echo PHP_EOL;
        }
        echo PHP_EOL;
    }
}
