<?php

namespace test;

require_once 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;
use src\AdventOfCode\Day7;

class Day7Test extends TestCase
{
    private $input = "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.";

    public function testFirstPartBaseExample()
    {
        $aoc = new Day7($this->input);
        $this->assertEquals('CABDFE', $aoc->firstPart());
    }

    public function testFirstPartSolution()
    {
        $aoc = new Day7();
        $this->assertEquals('ABGKCMVWYDEHFOPQUILSTNZRJX', $aoc->firstPart());
    }

    public function testSecondPartBaseExample()
    {
        $aoc = new Day7($this->input, 2, 0);
        $this->assertEquals(15, $aoc->secondPart());
    }

    public function testSecondPartSolution()
    {
        $aoc = new Day7();
        $this->assertEquals(898, $aoc->secondPart());
    }
}
