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
	fmt.Println(findDup(data, "\n"))
}

func findDup(str, sep string) (part1, sum int) {
	s := strings.Split(str, sep)
	m := make(map[int]int)
	foundPart1 := false
	m[0] = 1
	for i := 0; ; i++ {
		n, _ := strconv.Atoi(strings.TrimSpace(s[i]))
		sum += n
		m[sum]++
		if m[sum] == 2 {
			return
		}
		if i+1 == len(s) {
			i = -1
			if !foundPart1 {
				part1 = sum
				foundPart1 = true
			}
		}
	}
}
