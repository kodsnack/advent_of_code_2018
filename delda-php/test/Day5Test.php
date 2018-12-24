<?php

namespace test;

require_once 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;
use src\AdventOfCode\Day5;

class Day5Test extends TestCase
{
    private $input = "dabAcCaCBAcCcaDA";

    public function testDay5FirstPartBaseExample()
    {
        $aoc = new Day5($this->input);
        $this->assertEquals(10, $aoc->firstPart());
    }

    public function testDay5FirstPartSolution()
    {
        $aoc = new Day5();
        $this->assertEquals(11298, $aoc->firstPart());
    }

    public function testDay5SecondPartBaseExample()
    {
        $aoc = new Day5($this->input);
        $this->assertEquals(4, $aoc->secondPart());
    }

    public function testDay5SecondPartSolution()
    {
        $aoc = new Day5();
        $this->assertEquals(5148, $aoc->secondPart());
    }
}
