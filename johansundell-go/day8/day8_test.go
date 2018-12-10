package main

import (
	"fmt"
	"testing"
)

var input = `2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2`

func Test_Ex1(t *testing.T) {
	if _, n := makeNodes(parseInput(input), 0, node{}); n.getSum() != 138 || n.getSum2() != 66 {
		t.Fail()
	} else {
		fmt.Printf("%+v\n", n)
	}
}
