package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {

	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")

	grid := make([]int, 1500*1500)
	var id, x, y, w, h, c int

	for _, r := range rows {
		fmt.Sscanf(r, "#%d @ %d,%d: %dx%d", &id, &x, &y, &w, &h)
		for ix := x + 1; ix < x+1+w; ix++ {
			for iy := y + 1; iy < y+1+h; iy++ {
				grid[iy*1500+ix]++
			}
		}
	}

	for _, i := range grid {
		if i > 1 {
			c++
		}
	}
	fmt.Println(c)

	for _, r := range rows {
		fmt.Sscanf(r, "#%d @ %d,%d: %dx%d", &id, &x, &y, &w, &h)
		v := true
		for ix := x + 1; ix < x+1+w; ix++ {
			for iy := y + 1; iy < y+1+h; iy++ {
				if grid[iy*1500+ix] > 1 {
					v = false
				}
			}
		}
		if v {
			fmt.Println(id)
		}
	}

}
