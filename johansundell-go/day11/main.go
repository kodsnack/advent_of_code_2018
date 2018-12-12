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

func (c cell) getAreaSum(serial, dial int) (tot int) {
	for i := 0; i < dial; i++ {
		for n := 0; n < dial; n++ {
			tot += (cell{x: c.x + n, y: c.y + i}).power(serial)
		}
	}
	return
}

func main() {
	fmt.Println(getLargestPowerArea(4842, 3))
	fmt.Println(moveTheDial(4842))
}

func getLargestPowerArea(serial, dial int) (x, y, top int) {
	size := 300
	for i := 1; i < size-dial; i++ {
		for n := 1; n < size-dial; n++ {
			if sum := (cell{x: n, y: i}).getAreaSum(serial, dial); sum > top {
				x = n
				y = i
				top = sum
			}
		}
	}
	return
}

func moveTheDial(serial int) (x, y, top, dial int) {
	for i := 0; i < 25; i++ {
		if a, b, t := getLargestPowerArea(serial, i); t > top {
			top = t
			x = a
			y = b
			dial = i
		}
	}
	return
}
