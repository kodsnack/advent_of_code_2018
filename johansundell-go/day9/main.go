package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type stone struct {
	id   int
	prev *stone
	next *stone
}

func newStone(i int) *stone {
	s := &stone{}
	s.id = i
	s.next = s
	s.prev = s
	return s
}

func (s *stone) insert(stone *stone) *stone {
	next := s.next
	s.next = stone
	stone.prev = s
	stone.next = next
	next.prev = stone
	return stone
}

func (s *stone) remove() *stone {
	next := s.next
	s.next = next.next
	s.next.prev = s
	return next
}

func main() {
	data, err := adventofcode2017.GetInput("day9.txt")
	if err != nil {
		panic(err)
	}
	a, b := parseInput(data)
	fmt.Println(playGame(a, b))
	fmt.Println(playGame(a, b*100))
}

func parseInput(str string) (a, b int) {
	data := strings.Fields(str)
	a, _ = strconv.Atoi(data[0])
	b, _ = strconv.Atoi(data[6])
	return
}

func playGame(numElfes, last int) int {
	curr := newStone(0)
	elfes := make([]int, numElfes)
	for i := 0; i < last; i++ {
		if (i+1)%23 == 0 {
			for n := 0; n < 8; n++ {
				curr = curr.prev
			}
			s := curr.remove()
			curr = curr.next
			elfes[i%numElfes] += i + 1 + s.id
		} else {
			curr = curr.next
			curr = curr.insert(&stone{id: i + 1})
		}
	}
	sort.Sort(sort.Reverse(sort.IntSlice(elfes)))
	return elfes[0]
}
