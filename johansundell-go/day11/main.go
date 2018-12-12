package main

import (
	"fmt"
	"sort"
)

type cell struct {
	x, y int
}

type answer struct {
	x, y, top, dial int
}

func (c cell) power(serial int) int {
	rackId := c.x + 10
	i := ((rackId * c.y) + serial) * rackId
	return (i%1000)/100 - 5
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
	fmt.Printf("%+v\n", moveTheDialWithChannels(4842))
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

func getLargestPowerAreaWithChannel(serial, dial int, answ chan answer) {
	size := 300
	x, y, top := 0, 0, 0
	for i := 1; i < size-dial; i++ {
		for n := 1; n < size-dial; n++ {
			if sum := (cell{x: n, y: i}).getAreaSum(serial, dial); sum > top {
				x = n
				y = i
				top = sum
			}
		}
	}
	answ <- answer{x: x, y: y, top: top, dial: dial}
}

func moveTheDialWithChannels(serial int) answer {
	answ := make(chan answer)
	testSize := 300
	for i := 0; i < testSize; i++ {
		go getLargestPowerAreaWithChannel(serial, i, answ)
	}
	list := make([]answer, testSize)
	_ = list
	for i := 0; i < testSize; i++ {
		list[i] = (<-answ)
	}
	sort.Slice(list, func(i, j int) bool {
		return list[i].top > list[j].top
	})
	return list[0]
}
