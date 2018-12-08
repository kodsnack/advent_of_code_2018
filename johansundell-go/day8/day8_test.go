package main

import (
	"fmt"
	"testing"
)

var input = `2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2`

func Test_Ex1(t *testing.T) {
	res := parseInput(input)
	fmt.Println(res)
	n := makeNodes(res, 0)
	fmt.Println(check)
	return
	fmt.Println(n)
	n = makeNodes(res, 2)
	fmt.Println(n)

	n = makeNodes(res, n)
	fmt.Println(n)
	//return
	n = makeNodes(res, n)
	fmt.Println(n)

	n = makeNodes(res, n)
	fmt.Println(n)
}
