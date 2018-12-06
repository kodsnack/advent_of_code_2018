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
	//fmt.Println(data)
	parseInput(data, 32)
}
