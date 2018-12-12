package main

import (
	"fmt"
	"strconv"
	"strings"
)

type cell struct {
	x, y int
}

func (c cell) power(serial int) int {
	rackId := c.x + 10
	i := ((rackId * c.y) + serial) * rackId
	runes := strings.Split(fmt.Sprintf("%d", i), "")
	i, _ = strconv.Atoi(string(runes[len(runes)-3]))
	return i - 5
}

func (c cell) getAreaSum(serial int) (tot int) {
	for i := 0; i < 3; i++ {
		for n := 0; n < 3; n++ {
			tot += (cell{x: c.x + n, y: c.y + i}).power(serial)
		}
	}
	return
}

func main() {
	fmt.Println(getLargestPowerArea(4842))
}

func getLargestPowerArea(serial int) (x, y, top int) {
	for i := 1; i < 297; i++ {
		for n := 1; n < 297; n++ {
			if sum := (cell{x: n, y: i}).getAreaSum(serial); sum > top {
				x = n
				y = i
				top = sum
			}
		}
	}
	return
}
