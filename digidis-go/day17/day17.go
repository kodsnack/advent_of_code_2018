package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"time"
)

var (
	grid = make([][]byte, 2000)
	miny = 9999
	maxy = 0
)

func main() {
	start := time.Now()
	defer func() {
		fmt.Printf("All done in %v\n", time.Since(start))
	}()

	for i := range grid {
		grid[i] = make([]byte, 1000)
	}

	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")
	for _, r := range rows {
		if r[0] == 'x' {
			var x, y1, y2, o int
			// faster than fmt.Sscanf(r, "x=%d, y=%d..%d", &x, &y1, &y2)
			for i := range r {
				if r[i] == ',' {
					x = atoi(r[2:i])
					o = i + 4
				}
				if r[i] == '.' {
					y1 = atoi(r[o:i])
					y2 = atoi(r[i+2:])
					break
				}
			}
			if y1 < miny {
				miny = y1
			}
			if y2 > maxy {
				maxy = y2
			}
			for y := y1; y <= y2; y++ {
				grid[y][x] = '#'
			}
		} else {
			var y, x1, x2, o int
			// faster than fmt.Sscanf(r, "y=%d, x=%d..%d", &y, &x1, &x2)
			for i := range r {
				if r[i] == ',' {
					y = atoi(r[2:i])
					o = i + 4
				}
				if r[i] == '.' {
					x1 = atoi(r[o:i])
					x2 = atoi(r[i+2:])
					break
				}
			}
			if y < miny {
				miny = y
			}
			if y > maxy {
				maxy = y
			}
			for x := x1; x <= x2; x++ {
				grid[y][x] = '#'
			}
		}
	}

	fmt.Printf("Parsing took %v\n", time.Since(start))

	flowFrom(500, 0)

	tiles, water := 0, 0
	for y := miny; y <= maxy; y++ {
		for x := 0; x < len(grid[y]); x++ {
			if grid[y][x] == '~' || grid[y][x] == '|' {
				tiles++
			}
			if grid[y][x] == '~' {
				water++
			}
		}
	}

	fmt.Printf("y:[%d..%d]\n", miny, maxy)
	fmt.Printf("Tiles reached: %d, water retained: %d\n", tiles, water)
}

func flowFrom(wx, wy int) {

	// first fall down until clay or settled water is reached
	for {
		if wy > maxy {
			return
		}
		if wy >= miny && grid[wy][wx] == 0 {
			grid[wy][wx] = '|'
		}
		if grid[wy+1][wx] == '#' || grid[wy+1][wx] == '~' {
			break
		}
		wy++
	}

	leftBlock := -1
	// flow left
	for k := wx - 1; ; k-- {
		if grid[wy][k] == 0 {
			grid[wy][k] = '|'
		}
		if grid[wy+1][k] == '|' {
			// already been below here
			break
		}
		if grid[wy+1][k] == 0 {
			// continue downwards from here
			flowFrom(k, wy+1)
			break
		}
		if grid[wy][k] == '#' {
			leftBlock = k
			break
		}
	}
	// flow right
	for k := wx + 1; ; k++ {
		if grid[wy][k] == 0 {
			grid[wy][k] = '|'
		}
		if grid[wy+1][k] == '|' {
			// already been below here
			break
		}
		if grid[wy+1][k] == 0 {
			// continue downwards from here
			flowFrom(k, wy+1)
			break
		}
		if grid[wy][k] == '#' {
			if leftBlock != -1 {
				// fill this level and start over one level up
				for l := leftBlock + 1; l <= k-1; l++ {
					grid[wy][l] = '~'
				}
				flowFrom(wx, wy-1)
			}
			return
		}
	}
}

func atoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}
