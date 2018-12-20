<?php

namespace test;

require_once 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;
use src\AdventOfCode\Day4;

class Day4Test extends TestCase
{
    private $input = "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up";

    public function testFirstPartBaseExample()
    {
        $aoc = new Day4($this->input);
        $this->assertEquals(240, $aoc->firstPart());
    }

    public function testFirstPartSolution()
    {
        $aoc = new Day4();
        $this->assertEquals(106710, $aoc->firstPart());
    }

    public function testSecondPartBaseExample()
    {
        $aoc = new Day4($this->input);
        $this->assertEquals(4455, $aoc->secondPart());
    }

    public function testSecondPartSolution()
    {
        $aoc = new Day4();
        $this->assertEquals(10491, $aoc->secondPart());
    }
}
