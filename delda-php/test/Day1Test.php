<?php

namespace test;

require_once 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;
use src\AdventOfCode\Day1;

class Day1Test extends TestCase
{
    private $input = "+1\n-2\n+3\n+1";

    public function testFirstPartBaseExample()
    {
        $aoc = new Day1($this->input);
        $this->assertEquals(3, $aoc->firstPart());
    }

    public function testFirstPartSecondExample()
    {
        $aoc = new Day1("+1\n+1\n+1");
        $this->assertEquals(3, $aoc->firstPart());
    }

    public function testFirstPartThirdExample()
    {
        $aoc = new Day1("+1\n+1\n-2");
        $this->assertEquals(0, $aoc->firstPart());
    }

    public function testFirstPartFourthExample()
    {
        $aoc = new Day1("-1\n-2\n-3");
        $this->assertEquals(-6, $aoc->firstPart());
    }

    public function testFirstPartSolution()
    {
        $aoc = new Day1();
        $this->assertEquals(533, $aoc->firstPart());
    }

    public function testSecondPartBaseExample()
    {
        $aoc = new Day1($this->input);
        $this->assertEquals(2, $aoc->secondPart());
    }

    public function testSecondPartSecondExample()
    {
        $aoc = new Day1("+1\n-1");
        $this->assertEquals(1, $aoc->secondPart());
    }

    public function testSecondPartThirdExample()
    {
        $aoc = new Day1("+3\n+3\n+4\n-2\n-4");
        $this->assertEquals(10, $aoc->secondPart());
    }

    public function testSecondPartFourthExample()
    {
        $aoc = new Day1("-6\n+3\n+8\n+5\n-6");
        $this->assertEquals(5, $aoc->secondPart());
    }

    public function testSecondPartFifthExample()
    {
        $aoc = new Day1("+7\n+7\n-2\n-7\n-4");
        $this->assertEquals(14, $aoc->secondPart());
    }

    public function testSecondPartSolution()
    {
        $aoc = new Day1();
        $this->assertEquals(73272, $aoc->secondPart());
    }
}
