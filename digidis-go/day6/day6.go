package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type point struct {
	x, y int
}

var (
	coords     []*point
	counts     = make(map[int]int)
	inf        = make(map[int]bool)
	minx       = 9999
	miny       = 9999
	maxx, maxy int
	maxArea, r int
)

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")

	for _, r := range rows {
		var x, y int
		fmt.Sscanf(r, "%d, %d", &x, &y)
		coords = append(coords, &point{x, y})
		minx = min(minx, x)
		maxx = max(maxx, x)
		miny = min(miny, y)
		maxy = max(maxy, y)
	}

	for x := minx; x <= maxx; x++ {
		for y := miny; y <= maxy; y++ {
			if c := findClosest(x, y); c != -1 {
				if x == minx || x == maxx || y == miny || y == maxy {
					inf[c] = true
				}
				counts[c]++
			}
			t := totalDistance(x, y)
			if t < 10000 {
				r++
			}
		}
	}

	for k, v := range counts {
		if !inf[k] && v > maxArea {
			maxArea = v
		}
	}

	fmt.Printf("Largest area is %d\n", maxArea)
	fmt.Printf("Size of region with totalDistance < 10000 : %d\n", r)
}

func totalDistance(x, y int) (t int) {
	for _, c := range coords {
		dist := abs(c.x-x) + abs(c.y-y)
		t += dist
	}
	return t
}

func findClosest(x, y int) int {
	mi := -1
	m := 999999
	tied := -1
	for g, c := range coords {
		dist := abs(c.x-x) + abs(c.y-y)
		if dist == m && g != mi {
			tied = m
		}
		if dist < m {
			m = dist
			mi = g
		}
	}
	if tied == m {
		return -1
	}
	return mi
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
