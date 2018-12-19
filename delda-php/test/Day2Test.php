<?php

namespace test;

require_once 'vendor/autoload.php';

use PHPUnit\Framework\TestCase;
use src\AdventOfCode\Day2;

class Day2Test extends TestCase
{
    private $inputFirstPart = "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab";
    private $inputSecondPart = "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz";

    public function testFirstPartBaseExample()
    {
        $aoc = new Day2($this->inputFirstPart);
        $this->assertEquals(12, $aoc->firstPart());
    }

    public function testFirstPartSolution()
    {
        $aoc = new Day2();
        $this->assertEquals(3952, $aoc->firstPart());
    }

    public function testSecondPartBaseExample()
    {
        $aoc = new Day2($this->inputSecondPart);
        $this->assertEquals('fgij', $aoc->secondPart());
    }

    public function testSecondPartSolution()
    {
        $aoc = new Day2();
        $this->assertEquals('vtnikorkulbfejvyznqgdxpaw', $aoc->secondPart());
    }
}
