package main

import (
	"testing"
)

var data = `1, 1
1, 6
8, 3
3, 4
5, 5
8, 9`

func Test_Ex1(t *testing.T) {
	if p1, p2 := parseInput(data, 32); p1 != 17 || p2 != 16 {
		t.Fail()
	}
}
