<?php

namespace test;

require_once 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;
use src\AdventOfCode\Day6;

class Day6Test extends TestCase
{
    private $input = "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9";

    public function testFirstPartBaseExample()
    {
        $aoc = new Day6($this->input);
        $this->assertEquals(17, $aoc->firstPart());
    }

    public function testFirstPartSolution()
    {
        $aoc = new Day6();
        $this->assertEquals(4887, $aoc->firstPart());
    }

    public function testSecondPartBaseExample()
    {
        $aoc = new Day6($this->input);
        $this->assertEquals(16, $aoc->secondPart(32));
    }

    public function testSecondPartSolution()
    {
        $aoc = new Day6();
        $this->assertEquals(34096, $aoc->secondPart());
    }
}
