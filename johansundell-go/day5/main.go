package main

import (
	"fmt"
	"math"
	"sort"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, err := adventofcode2017.GetInput("day5.txt")
	if err != nil {
		panic(err)
	}
	answ1 := parseString(data)
	fmt.Println(len(answ1))
	fmt.Println(part2(answ1))

}

func parseString(str string) string {
	found := false
	for str, found = findTheRunes(str); found == true && len(str) > 1; {
		str, found = findTheRunes(str)
	}
	return str
}

func findTheRunes(str string) (string, bool) {
	for i := 0; i < len(str)-1; i++ {
		if t := math.Abs(float64(str[i] - str[i+1])); t == 32 || t == 224 {
			str = strings.Replace(str, string(str[i])+string(str[i+1]), "", 1)
			return str, true
		}
	}
	return str, false
}

func part2(str string) int {
	m := make(map[string]int)
	for _, r := range str {
		m[strings.ToUpper(string(r))] = 0
	}
	i := make([]int, 0)
	for k, _ := range m {
		s := strings.Replace(str, strings.ToUpper(k), "", -1)
		s = strings.Replace(s, strings.ToLower(k), "", -1)
		n := len(parseString(s))
		i = append(i, n)
		m[k] = n
	}
	sort.Ints(i)
	return i[0]
}
