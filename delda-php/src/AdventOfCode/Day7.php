<?php

namespace src\AdventOfCode;

use stdClass;

class Day7 extends AbstractAdventOfCode
{
    private $numWorkers;
    private $baseTime;
    protected $class = 7;

    public function __construct($input = null, int $numWorkers = 5, int $baseTime = 60)
    {
        parent::__construct($input);
        $this->numWorkers = $numWorkers;
        $this->baseTime = $baseTime;
    }

    public function firstPart(): string
    {
        [$graph, $size] = $this->parse($this->input);

        $starterNode = [];
        foreach ($graph as $node => $values) {
            $starterNode = array_merge($starterNode, $this->getAvailable($graph, [$node]));
        }
        array_unique($starterNode);

        return $this->assemble($graph, $size, $starterNode);
    }

    public function secondPart(): string
    {
        [$graph, $size] = $this->parse($this->input);

        $list = [];
        foreach ($graph as $node => $values) {
            $list = array_merge($list, $this->getAvailable($graph, [$node]));
        }
        array_unique($list);

        $numWorkers = $this->numWorkers;
        for ($i = 0; $i < $numWorkers; $i++) {
            $tmpClass = new stdClass();
            $tmpClass->time = 0;
            $tmpClass->node = null;
            $workers[] = $tmpClass;
        }

        $done = '';
        $time = 0;
        do {
            foreach ($workers as $worker => $step) {
                if ($this->isFree($time, $this->baseTime, $worker, $workers)) {
                    $node = $workers[$worker]->node;
                    if ($node) {
                        $done .= $node;
                        if (isset($graph[$node])) {
                            foreach ($graph[$node] as $key => $value) {
                                if ($graph[$node][$key]) {
                                    $graph[$node][$key] = 2;
                                }
                            }
                        }
                        $workers[$worker]->node = null;
                    }
                }
            }
            $areAllWorkersFree = true;
            foreach ($workers as $worker => $step) {
                if ($this->isFree($time, $this->baseTime, $worker, $workers)) {
                    $this->assignNode($graph, $size, $list, $workers, $worker, $time);
                }
                if ($step->node) {
                    $areAllWorkersFree = false;
                }
            }
            $time++;
        } while (!$areAllWorkersFree || !empty($list));

        return $time - 1;
    }

    public function parse(string $input): array
    {
        $counter = 0;
        $graph = array_reduce(explode(PHP_EOL, $input), function ($carry, $value) use (&$counter) {
            preg_match('/Step ([A-Z]) must be finished before step ([A-Z]) can begin./', $value, $matches);
            $carry[$matches[1]][$matches[2]] = 1;

            return $carry;
        });

        $tmp = array_reduce(array_keys($graph), function ($carry, $item) use ($graph) {
            if (empty($carry) || !in_array($item, $carry)) {
                $carry[] = $item;
            }
            if (!empty($carry)) {
                foreach ($graph[$item] as $key => $value) {
                    if (!in_array($key, $carry)) {
                        $carry[] = $key;
                    }
                }
            }
            return $carry;
        });

        return [$graph, sizeof($tmp)];
    }

    public function assemble(array $graph, int $size, array $list): string
    {
        $list = array_unique($list);
        sort($list);
        ksort($list);

        for ($i = 0; $i < sizeof($list); $i++) {
            if ($this->checkAvailable($graph, $size, $list[$i])) {
                $node = $list[$i];
                unset($list[$i]);
                if (isset($graph[$node])) {
                    foreach ($graph[$node] as $key => $value) {
                        if ($graph[$node][$key]) {
                            $list[] = $key;
                            $graph[$node][$key] = 2;
                        }
                    }
                }
                return $node . $this->assemble($graph, $size, $list);
            }
        }

        return '';
    }

    private function getAvailable(array $graph, array $node)
    {
        $availableNodes = [];
        $isInDegree = false;
        $size = sizeof($graph);
        foreach ($node as $yNode) {
            $isInDegree = false;
            for ($i = 65; $i <= (65 + $size); $i++) {
                if (!empty($graph[chr($i)][$yNode]) && $graph[chr($i)][$yNode] == 1) {
                    $isInDegree = true;
                }
            }
            if ($isInDegree === false) {
                $availableNodes[] = $yNode;
            }
        }

        return $availableNodes;
    }

    private function checkAvailable(array $graph, int $size, string $node)
    {
        for ($i = 65; $i < (65 + $size); $i++) {
            if (isset($graph[chr($i)][$node]) && $graph[chr($i)][$node] === 1) {
                return false;
            }
        }

        return true;
    }

    private function isFree(int $time, int $baseTime, int $worker, array $workers)
    {
        if ($workers[$worker]->node) {
            $waitTime = ord($workers[$worker]->node) - 65 + 1 + $baseTime;
            if ($workers[$worker]->time + $waitTime > $time) {
                return false;
            }
        }

        return true;
    }

    private function assignNode(array &$graph, int $size, array &$list, array $workers, string $worker, int $time)
    {
        for ($i = 0; $i < sizeof($list); $i++) {
            if ($this->checkAvailable($graph, $size, $list[$i])) {
                $node = $list[$i];
                unset($list[$i]);
                $workers[$worker]->node = $node;
                $workers[$worker]->time = $time;
                if (isset($graph[$node])) {
                    foreach ($graph[$node] as $key => $value) {
                        if ($graph[$node][$key]) {
                            $list[] = $key;
                        }
                    }
                }
                $list = array_unique($list);
                sort($list);
                ksort($list);

                return $node;
            }
        }

        return '';
    }

    private function printGraph(array $graph, int $size)
    {
        echo PHP_EOL, '  ';
        for ($i = 65; $i < (65 + $size); $i++) {
            echo chr($i);
        }
        echo PHP_EOL;
        for ($x = 65; $x < (65 + $size); $x++) {
            echo chr($x), ' ';
            for ($y = 65; $y <= (65 + $size); $y++) {
                if (isset($graph[chr($x)][chr($y)])) {
                    echo $graph[chr($x)][chr($y)];
                } else {
                    echo ' ';
                }
            }
            echo PHP_EOL;
        }
        echo PHP_EOL;
    }

    private function printStep(array $workers, int $time, string $done)
    {
        echo PHP_EOL, $time, "\t";
        foreach ($workers as $worker) {
            if (empty($worker->node)) {
                echo '.';
            } else {
                echo $worker->node;
            }
            echo "\t";
        }
        echo $done;
    }
}
