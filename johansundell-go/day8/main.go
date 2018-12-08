package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type node struct {
	id       int
	children []node
	meta     []int
}

var check = 0

func main() {
	data, err := adventofcode2017.GetInput("day8.txt")
	if err != nil {
		panic(err)
	}
	//fmt.Println(parseData(data, 60))
	list := parseInput(data)
	makeNodes(list, 0)
	fmt.Println(check)
}

func parseInput(str string) (list []int) {
	strList := strings.Fields(str)
	for _, r := range strList {
		i, _ := strconv.Atoi(r)
		list = append(list, i)
	}
	return list
}

func makeNodes(list []int, pos int) int {
	kids := list[pos]
	metaNum := list[pos+1]
	pos += 2

	for i := 0; i < kids; i++ {
		pos = makeNodes(list, pos)
	}

	for i := 0; i < metaNum; i++ {
		//fmt.Println(list[pos])
		check += list[pos]
		pos++
	}
	/*if kids == 0 {
	}*/
	return pos
}
