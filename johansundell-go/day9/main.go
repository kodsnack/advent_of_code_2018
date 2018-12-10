package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
	"github.com/johansundell/advent_of_code_2018/johansundell-go/day9/ringints"
)

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
	curr := ringints.New()
	elfes := make([]int, numElfes)
	for i := 1; i <= last; i++ {
		if (i)%23 == 0 {
			for n := 0; n < 8; n++ {
				curr = curr.Prev
			}
			s := curr.RemoveNext()
			curr = curr.Next
			elfes[i%numElfes] += i + s.Id
		} else {
			curr = curr.Next.Insert(&ringints.RingInt{Id: i})
		}
	}
	sort.Sort(sort.Reverse(sort.IntSlice(elfes)))
	return elfes[0]
}
