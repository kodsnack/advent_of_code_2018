<?php

namespace src\AdventOfCode;

class Day4 extends AbstractAdventOfCode
{
    protected $class = 4;

    public function firstPart(): string
    {
        $records = $this->parse($this->input);
        $minutesAsleep = $this->countMinutesAsleep($records);
        $sleepyheadId = $this->sleepyhead($minutesAsleep);
        $maxAsleep = 0;
        foreach ($sleepyheadId as $guardId) {
            $asleepMost = $this->asleepMost($minutesAsleep[$guardId]);
            $result = $guardId * $asleepMost;
            if ($maxAsleep < $result) {
                $maxAsleep = $result;
            }
        }

        return $maxAsleep;
    }

    public function secondPart(): string
    {
        $records = $this->parse($this->input);
        $minutesAsleep = $this->countMinutesAsleep($records);
        $maxMinutes = $this->mostFrequentlyAsleep($minutesAsleep);
        $maxId = 0;
        $maxValue = 0;
        foreach ($maxMinutes as $id => $values) {
            $max = max($values);
            if ($maxValue < $max) {
                $maxValue = $max;
                $maxId = $id;
            }
        }

        $max = 0;
        foreach ($maxMinutes[$maxId] as $key => $value) {
            $max = max([$max , $maxId * $key]);
        }

        return $max;
    }

    private function parse(string $input): array
    {
        $records = [];
        foreach (explode(PHP_EOL, $input) as $record) {
            preg_match('/\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (.*)/', $record, $matches);
            $records[strtotime($matches[1])] = $matches[0];
        }
        ksort($records);

        return $records;
    }

    private function countMinutesAsleep(array $records): array
    {
        $minutesAsleep = [];
        foreach ($records as $date => $record) {
            preg_match('/(\d{2}):(\d{2})/', $record, $time);
            $hour = (int)$time[1];
            $minute = (int)$time[2];
            if (preg_match('/Guard #(\d+)/', $record, $matches)) {
                $id = (int)$matches[1];
                if ($hour == 23) {
                    $lastMinute = 0;
                } else {
                    $lastMinute = $minute;
                }
                if (!isset($minutesAsleep[$id])) {
                    $minutesAsleep[$id] = array_fill(0, 59, 0);
                }
            } elseif (preg_match('/falls asleep/', $record)) {
                $lastMinute = $minute;
            } elseif (preg_match('/wakes up/', $record)) {
                for ($i = $lastMinute; $i < $minute; $i++) {
                    $minutesAsleep[$id][$i]++;
                }
            }
        }

        return $minutesAsleep;
    }

    private function sleepyhead(array $minutesAsleep): array
    {
        $maxAsleep = 0;
        $sleepyheadId = null;
        foreach ($minutesAsleep as $id => $asleepTime) {
            $asleep = sizeof(array_filter($asleepTime, function ($value) {
                return $value > 0;
            }));
            if ($maxAsleep < $asleep) {
                $maxAsleep = $asleep;
                $sleepyheadId = [];
                $sleepyheadId[] = $id;
            } elseif ($maxAsleep == $asleep) {
                $sleepyheadId[] = $id;
            }
        }

        return $sleepyheadId;
    }

    private function asleepMost(array $guardAsleep): int
    {
        $maxs =  array_keys($guardAsleep, max($guardAsleep));
        return $maxs[0];
    }

    private function mostFrequentlyAsleep($orders)
    {
        $return = [];
        foreach ($orders as $id => $order) {
            $max = max($orders[$id]);
            $return[$id] = array_filter($orders[$id], function ($value) use ($max) {
                return $value == $max;
            });
        }

        return $return;
    }
}
