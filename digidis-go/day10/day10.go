package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type point struct {
	x, y, dx, dy int
}

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")
	var (
		x, y, dx, dy int
		points       []*point
	)
	for _, r := range rows {
		r = strings.Replace(r, " ", "", -1)
		fmt.Sscanf(r, "position=<%d,%d>velocity=<%d,%d>", &x, &y, &dx, &dy)
		points = append(points, &point{
			x, y, dx, dy,
		})
	}

	second := 1
	centered := false
	for {
		minx, maxx, miny, maxy := 0, 0, 0, 0
		for _, p := range points {
			p.x += p.dx
			p.y += p.dy
			if minx == 0 || p.x < minx {
				minx = p.x
			}
			if p.x > maxx {
				maxx = p.x
			}
			if miny == 0 || p.y < miny {
				miny = p.y
			}
			if p.y > maxy {
				maxy = p.y
			}
		}
		if maxy-miny < 30 {
			centered = true
			fmt.Printf("[Second %d] Min x: %d, Max x: %d, min y: %d, max y: %d => \n", second, minx, maxx, miny, maxy)
			for y := miny; y <= maxy; y++ {
				for x := minx; x <= maxx; x++ {
					found := false
					for _, p := range points {
						if p.x == x && p.y == y {
							found = true
							break
						}
					}
					if found {
						fmt.Printf("#")
					} else {
						fmt.Printf(".")
					}
				}
				fmt.Printf("\n")
			}
		} else {
			if centered {
				return
			}
		}
		second++
	}
}
