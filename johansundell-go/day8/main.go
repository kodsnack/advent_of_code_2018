package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type node struct {
	children []node
	meta     []int
	sum      int
}

func (n node) getSum() int {
	s := n.sum
	for i := 0; i < len(n.children); i++ {
		s += n.children[i].getSum()
	}
	return s
}

func (n node) getSum2() int {
	if len(n.children) == 0 {
		return n.sum
	}
	s := 0
	for i := 0; i < len(n.meta); i++ {
		if n.meta[i] != 0 && n.meta[i] <= len(n.children) {
			s += n.children[n.meta[i]-1].getSum2()
		}
	}
	return s
}

func main() {
	data, err := adventofcode2017.GetInput("day8.txt")
	if err != nil {
		panic(err)
	}
	_, n := makeNodes(parseInput(data), 0, node{})
	fmt.Println(n.getSum(), n.getSum2())
}

func parseInput(str string) (list []int) {
	strList := strings.Fields(str)
	for _, r := range strList {
		i, _ := strconv.Atoi(r)
		list = append(list, i)
	}
	return list
}

func makeNodes(list []int, pos int, n node) (int, node) {
	kids := list[pos]
	metaNum := list[pos+1]
	pos += 2
	for i := 0; i < kids; i++ {
		child := node{}
		pos, child = makeNodes(list, pos, child)
		n.children = append(n.children, child)
	}

	for i := 0; i < metaNum; i++ {
		n.sum += list[pos]
		n.meta = append(n.meta, list[pos])
		pos++
	}
	return pos, n
}
