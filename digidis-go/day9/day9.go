package main

import (
	"fmt"
	"time"
)

const (
	players = 428
	last    = 7082500
)

type marble struct {
	next int
	prev int
}

var marbles []marble

func main() {
	var (
		current = 0
		scores  = make([]int, players+1)
		c       = 1
	)
	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	marbles = make([]marble, last+1)
	for {
		for p := 1; p <= players; p++ {
			if c%23 == 0 {
				scores[p] += c
				for i := 0; i < 6; i++ {
					current = marbles[current].prev
				}
				scores[p] += marbles[current].prev
				remove(marbles[current].prev)
			} else {
				insertAfter(marbles[current].next, c)
				current = c
			}
			c++
			if c > last {
				m := 0
				for _, s := range scores {
					if s > m {
						m = s
					}
				}
				fmt.Printf("High score: %d\n", m)
				return
			}
		}
	}
}

func insertAfter(p int, m int) {
	marbles[m].next, marbles[p].next = marbles[p].next, m
	marbles[m].prev = p
	marbles[marbles[m].next].prev = m
}

func remove(m int) {
	marbles[marbles[m].prev].next = marbles[m].next
	marbles[marbles[m].next].prev = marbles[m].prev
}
