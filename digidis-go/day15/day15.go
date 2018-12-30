package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"
)

type step struct {
	x, y   int
	mx, my int
}

func main() {
	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	var (
		grid    = make([][]byte, 32)
		hp      = make([][]byte, 32)
		E       = 0
		G       = 0
		eAttack = 3
	)
	if len(os.Args) > 1 {
		eAttack, _ = strconv.Atoi(os.Args[1])
	}

	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")
	for i, r := range rows {
		grid[i] = make([]byte, len(r))
		hp[i] = make([]byte, len(r))
		copy(grid[i], r)
		for x := range grid[i] {
			if grid[i][x] == 'E' {
				E++
				hp[i][x] = 200
			}
			if grid[i][x] == 'G' {
				G++
				hp[i][x] = 200
			}
		}
	}

	completedRounds := 0
game:
	for {
		done := make(map[string]bool)
		for y := 0; y < len(grid); y++ {
			for x := 0; x < len(grid[y]); x++ {
				if done[fmt.Sprintf("%d,%d", x, y)] {
					continue
				}
				self := grid[y][x]
				if self != 'G' && self != 'E' {
					continue
				}
				px, py := x, y
				var target byte
				if self == 'G' {
					target = 'E'
				} else {
					target = 'G'
				}
				steps := []step{
					step{x, y - 1, x, y - 1},
					step{x - 1, y, x - 1, y},
					step{x + 1, y, x + 1, y},
					step{x, y + 1, x, y + 1},
				}
				var nextSteps []step
				visited := make(map[string]bool)
				var bestMoves []step
				c := 0
			breadthSearch:
				for bestMoves == nil && len(steps) > 0 {
					for _, p := range steps {
						if visited[fmt.Sprintf("%d,%d", p.x, p.y)] == true {
							continue
						}
						switch grid[p.y][p.x] {
						case '#', self:
							continue
						case target:
							if c == 0 {
								break breadthSearch
							} else {
								bestMoves = append(bestMoves, p)
							}
						case '.':
							// add next moves
							visited[fmt.Sprintf("%d,%d", p.x, p.y)] = true
							nextSteps = append(nextSteps, step{p.x, p.y - 1, p.mx, p.my})
							nextSteps = append(nextSteps, step{p.x - 1, p.y, p.mx, p.my})
							nextSteps = append(nextSteps, step{p.x + 1, p.y, p.mx, p.my})
							nextSteps = append(nextSteps, step{p.x, p.y + 1, p.mx, p.my})
						}
					}
					steps, nextSteps = nextSteps, steps
					nextSteps = nextSteps[:0]
					c++
				}

				// make move, if any
				if bestMoves != nil {
					sort.Sort(ByReadingOrder(bestMoves))

					grid[y][x] = '.'
					px, py = bestMoves[0].mx, bestMoves[0].my
					grid[py][px] = self
					done[fmt.Sprintf("%d,%d", px, py)] = true

					hp[py][px] = hp[y][x]
					hp[y][x] = 0
				}

				// attack? check targets in reading order
				low, attackX, attackY := 999, -1, -1
				if grid[py-1][px] == target {
					low = int(hp[py-1][px])
					attackX, attackY = px, py-1
				}
				if grid[py][px-1] == target {
					if int(hp[py][px-1]) < low {
						low = int(hp[py][px-1])
						attackX, attackY = px-1, py
					}
				}
				if grid[py][px+1] == target {
					if int(hp[py][px+1]) < low {
						low = int(hp[py][px+1])
						attackX, attackY = px+1, py
					}
				}
				if grid[py+1][px] == target {
					if int(hp[py+1][px]) < low {
						attackX, attackY = px, py+1
					}
				}
				if attackX == -1 {
					continue
				}
				if self == 'E' {
					if hp[attackY][attackX] <= byte(eAttack) {
						fmt.Printf("Slained a gobling\n")
						G--
						grid[attackY][attackX] = '.'
						hp[attackY][attackX] = 0
					} else {
						hp[attackY][attackX] -= byte(eAttack)
					}
				} else {
					if hp[attackY][attackX] <= 3 {
						fmt.Printf("Slained an elv\n")
						E--
						grid[attackY][attackX] = '.'
						hp[attackY][attackX] = 0
					} else {
						hp[attackY][attackX] -= 3
					}
				}
				if E == 0 || G == 0 {
					break game
				}
			}
		}
		completedRounds++
	}

	fmt.Printf("Game over: elves: %d, goblins: %d. %d rounds completed\n", E, G, completedRounds)
	sum := 0
	for _, row := range hp {
		for _, v := range row {
			if v > 0 {
				sum += int(v)
			}
		}
	}
	fmt.Printf("hp sum: %d, outcome: %d\n", sum, sum*completedRounds)

}

type ByReadingOrder []step

func (a ByReadingOrder) Len() int      { return len(a) }
func (a ByReadingOrder) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a ByReadingOrder) Less(i, j int) bool {
	// first sort by target position
	if a[i].y != a[j].y {
		return a[i].y < a[j].y
	}
	if a[i].x != a[j].x {
		return a[i].x < a[j].x
	}
	// then move order
	if a[i].my != a[j].my {
		return a[i].my < a[j].my
	}
	return a[i].mx < a[j].mx
}
