<?php

namespace src\AdventOfCode;

class Day6 extends AbstractAdventOfCode
{
    protected $class = 6;

    const minimumTotalDistance = 10000;

    public function firstPart(): string
    {
        $input = $this->parse($this->input);
        $maxCoord = $this->maxCoord($input);

        $areas = $count = [];
        for ($y = 0; $y <= $maxCoord[1] + 1; $y++) {
            for ($x = 0; $x <= $maxCoord[0] + 1; $x++) {
                $areas[$x][$y] = $this->minManhattanDistance([$x, $y], $input);
            }
        }

        for ($y = 0; $y <= $maxCoord[1] + 1; $y++) {
            for ($x = 0; $x <= $maxCoord[0] + 1; $x++) {
                $key = $areas[$x][$y];
                if (!isset($count[$key])) {
                    $count[$key] = 0;
                }
                $count[$key]++;
            }
        }

        for ($y = 0; $y < $maxCoord[1] + 1; $y++) {
            $key = $areas[0][$y];
            unset($count[$key]);
            $key = $areas[$maxCoord[0]+1][$y];
            unset($count[$key]);
        }
        for ($x = 0; $x < $maxCoord[0] + 1; $x++) {
            $key = $areas[$x][0];
            unset($count[$key]);
            $key = $areas[$x][$maxCoord[1]+1];
            unset($count[$key]);
        }

        return max($count);
    }

    public function secondPart(int $minimumTotalDistance = null): string
    {
        $minimumTotalDistance = $minimumTotalDistance ?: self::minimumTotalDistance;

        $input = $this->parse($this->input);
        $maxCoord = $this->maxCoord($input);

        for ($y = 0; $y <= $maxCoord[1] + 1; $y++) {
            for ($x = 0; $x <= $maxCoord[0] + 1; $x++) {
                $sumManhattanDistance = $this->sumManhattanDistance([$x, $y], $input);
                $areas[$x][$y] = ($sumManhattanDistance < $minimumTotalDistance)
                    ? 1
                    : 0;
            }
        }

        $count = 0;
        for ($y = 0; $y <= $maxCoord[1] + 1; $y++) {
            for ($x = 0; $x <= $maxCoord[0] + 1; $x++) {
                $count += $areas[$x][$y] ? 1 : 0;
            }
        }

        return $count;
    }

    private function parse(string $input): array
    {
        return array_reduce(explode(PHP_EOL, $input), function ($carry, $item) {
            $carry[] = explode(', ', $item);
            return $carry;
        });
    }

    private function maxCoord(array $input): array
    {
        return array_reduce($input, function ($carry, $item) {
            $carry[0] = max($carry[0], $item[0]);
            $carry[1] = max($carry[1], $item[1]);
            return $carry;
        }, [0, 0]);
    }

    private function minManhattanDistance(array $coordBase, array $input): int
    {
        $min = PHP_INT_MAX;
        foreach ($input as $key => $coord) {
            $currentMin = $this->manhattanDistance($coordBase, $coord);
            if ($min > $currentMin) {
                $min = $currentMin;
                $minKey = $key;
            } elseif ($min == $currentMin) {
                $minKey = -1;
            }
        }

        return $minKey;
    }

    private function sumManhattanDistance(array $coordBase, array $input): int
    {
        $sum = 0;
        foreach ($input as $key => $coord) {
            $sum += $this->manhattanDistance($coordBase, $coord);
        }

        return $sum;
    }

    private function manhattanDistance(array $coordA, array $coordB): int
    {
        return abs($coordA[0] - $coordB[0]) + abs($coordA[1] - $coordB[1]);
    }
}
