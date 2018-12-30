<?php

namespace test;

require_once 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;
use src\AdventOfCode\Day3;

class Day3Test extends TestCase
{
    private $input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2";

    public function testFirstPartBaseExample()
    {
        $aoc = new Day3($this->input);
        $this->assertEquals(4, $aoc->firstPart(10));
    }

    public function testFirstPartSolution()
    {
        $aoc = new Day3();
        $this->assertEquals(104439, $aoc->firstPart());
    }

    public function testSecondPartBaseExample()
    {
        $aoc = new Day3($this->input);
        $this->assertEquals(3, $aoc->secondPart(10));
    }

    public function testSecondPartSolution()
    {
        $aoc = new Day3();
        $this->assertEquals(701, $aoc->secondPart());
    }
}
