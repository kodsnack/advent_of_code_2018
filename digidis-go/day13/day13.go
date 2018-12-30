package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"sort"
	"strings"
)

type cart struct {
	dir      int
	nextTurn int
	x, y     int
	crashed  bool
}

var (
	grid  = make([][]byte, 150)
	carts []*cart
)

func main() {

	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")
	for i, r := range rows {
		grid[i] = make([]byte, len(r))
		copy(grid[i], r)

		for _, c := range findAll(grid[i], '<') {
			carts = append(carts, &cart{dir: 3, x: c, y: i})
			grid[i][c] = '-'
		}
		for _, c := range findAll(grid[i], '>') {
			carts = append(carts, &cart{dir: 1, x: c, y: i})
			grid[i][c] = '-'
		}
		for _, c := range findAll(grid[i], '^') {
			carts = append(carts, &cart{dir: 0, x: c, y: i})
			grid[i][c] = '|'
		}
		for _, c := range findAll(grid[i], 'v') {
			carts = append(carts, &cart{dir: 2, x: c, y: i})
			grid[i][c] = '|'
		}
	}

	steps := 1
	cartsLeft := len(carts)
	for {
		sort.Sort(ByLocation(carts))

	cartLoop:
		for me, c := range carts {
			if c.crashed {
				continue
			}

			switch c.dir {
			case 0:
				c.y--
			case 1:
				c.x++
			case 2:
				c.y++
			case 3:
				c.x--
			}

			switch grid[c.y][c.x] {
			case ' ':
				panic("Cart out of track!")
			case '/':
				switch c.dir {
				case 0, 2:
					c.dir++
				case 1, 3:
					c.dir--
				}
			case '\\':
				switch c.dir {
				case 0, 2:
					c.dir--
				case 1, 3:
					c.dir++
				}
			case '+':
				switch c.nextTurn {
				case 0:
					// turn left
					c.dir--
					c.nextTurn = 1
				case 1:
					// straight ahead
					c.nextTurn = 2
				case 2:
					// turn right
					c.dir++
					c.nextTurn = 0
				}
			}
			c.dir = (c.dir + 4) % 4

			for k, c2 := range carts {
				if !c2.crashed && k != me && c.x == c2.x && c.y == c2.y {
					c.crashed = true
					c2.crashed = true
					cartsLeft -= 2
					fmt.Printf("BOOM! Collision at (%d,%d) between %d and %d at step %d, %d carts left\n", c.x, c.y, me, k, steps, cartsLeft)
					continue cartLoop
				}
			}
		}

		if cartsLeft == 1 {
			for k, c := range carts {
				if !c.crashed {
					fmt.Printf("Last cart is %d at position %d,%d at step %d\n", k, c.x, c.y, steps)
				}
			}
			return
		}
		steps++
	}
}

func findAll(r []byte, b byte) (result []int) {
	p := 0
	for {
		j := bytes.Index(r[p:], []byte{b})
		if j == -1 {
			break
		}
		result = append(result, p+j)
		p += (j + 1)
	}
	return
}

type ByLocation []*cart

func (a ByLocation) Len() int           { return len(a) }
func (a ByLocation) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByLocation) Less(i, j int) bool { return a[i].y < a[j].y && a[i].x < a[j].x }
