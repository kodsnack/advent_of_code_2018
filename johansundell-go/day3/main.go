package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type claim struct {
	id     string
	left   int
	top    int
	width  int
	height int
}

var pos map[posType]int

type posType struct {
	x, y int
}

type claims []claim

func init() {
	pos = make(map[posType]int)
}

func main() {
	data, err := adventofcode2017.GetInput("day3.txt")
	if err != nil {
		panic(err)
	}
	c := claims{}
	for _, str := range strings.Split(data, "\n") {
		c = append(c, parseClaim(str))
	}
	fmt.Println(findThem())
	fmt.Println(findTheSantaSuit(c))
}

func findTheSantaSuit(c claims) string {
	for _, row := range c {
		if !row.hasConflict() {
			return row.id
		}
	}
	return ""
}

func parseClaim(str string) claim {
	res := strings.Split(str, " ")
	res[2] = strings.Replace(res[2], ":", "", -1)
	c := claim{}
	c.id = res[0]
	t := strings.Split(res[2], ",")
	c.left, _ = strconv.Atoi(t[0])
	c.top, _ = strconv.Atoi(t[1])
	t = strings.Split(res[3], "x")
	c.width, _ = strconv.Atoi(t[0])
	c.height, _ = strconv.Atoi(t[1])
	for i := c.top; i < c.top+c.height; i++ {
		for n := c.left; n < c.left+c.width; n++ {
			pos[posType{x: i, y: n}]++
		}
	}
	return c
}

func (a claim) hasConflict() bool {
	for i := a.top; i < a.top+a.height; i++ {
		for n := a.left; n < a.left+a.width; n++ {
			if pos[posType{x: i, y: n}] > 1 {
				return true
			}
		}
	}
	return false

}

func findThem() (a int) {
	for i := 0; i < 1000; i++ {
		for n := 0; n < 1000; n++ {
			if pos[posType{x: i, y: n}] > 1 {
				a++
			}
		}
	}
	return
}
