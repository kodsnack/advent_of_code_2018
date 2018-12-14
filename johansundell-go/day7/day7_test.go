package main

import (
	"testing"
)

var data = `Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.`

func Test_Ex1(t *testing.T) {
	if str, time := parseData(data, 0); str != "CABDFE" || time != 14 {
		t.Fail()
	}
}
