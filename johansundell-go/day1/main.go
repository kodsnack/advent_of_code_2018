package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, err := adventofcode2017.GetInput("day1.txt")
	if err != nil {
		panic(err)
	}
	fmt.Println(makeInts(data, "\n"), findDup(data, "\n"))
}

func makeInts(str, sep string) int {
	strArr := strings.Split(str, sep)
	sum := 0
	for _, i := range strArr {
		i = strings.TrimSpace(i)
		n, _ := strconv.Atoi(i)
		sum += n
	}
	return sum
}

func findDup(str, sep string) int {
	s := strings.Split(str, sep)
	m := make(map[int]int)
	sum := 0
	m[0] = 1
	for i := 0; ; i++ {
		x := strings.TrimSpace(s[i])
		n, _ := strconv.Atoi(x)
		sum += n
		m[sum]++
		if m[sum] == 2 {
			return sum
		}
		if i+1 == len(s) {
			i = -1
		}
	}
}
