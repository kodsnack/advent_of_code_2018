package main

import (
	"fmt"
	"time"
)

const input = 7803

func main() {

	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	grid := make([]int, 301*301)
	for y := 1; y <= 300; y++ {
		for x := 1; x <= 300; x++ {
			rackID := x + 10
			power := rackID * (rackID*y + input)
			grid[300*y+x] = (power%1000)/100 - 5
		}
	}

	var m int

	for y := 1; y <= 300; y++ {
		for x := 1; x <= 300; x++ {
			t := grid[300*y+x]
			for g := 2; g <= 300-max(x, y)+1; g++ {
				// add right column
				xi := x + g - 1
				for yi := y; yi < y+g-1; yi++ {
					t += grid[300*yi+xi]
				}
				// add bottom column
				yi := y + g - 1
				for xi := x; xi < x+g-1; xi++ {
					t += grid[300*yi+xi]
				}
				// add bottom right corner
				t += grid[300*(y+g-1)+(x+g-1)]

				if t > m {
					m = t
					fmt.Printf("New max %d at %d,%d,%d\n", m, x, y, g)
				}
			}
		}
	}

}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
