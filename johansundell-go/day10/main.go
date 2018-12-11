package main

import (
	"bufio"
	"fmt"
	"math"
	"sort"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type pair struct {
	x, y int
}

type point struct {
	position pair
	velocity pair
}

func newPoint(x, y, moveX, moveY int) (p point) {
	p.position = pair{x: x, y: y}
	p.velocity = pair{x: moveX, y: moveY}
	return
}

func (p *point) move() point {
	p.position.x += p.velocity.x
	p.position.y += p.velocity.y
	//fmt.Println(p)
	return *p
}

func (p *point) back() point {
	p.position.x -= p.velocity.x
	p.position.y -= p.velocity.y
	return *p
}

type areaMap map[pair]bool

type points []point

func getMinValues(list points) (diffX, diffY int) {
	sort.Slice(list, func(i, j int) bool { return list[i].position.y < list[j].position.y })
	minY := list[0].position.y
	sort.Slice(list, func(i, j int) bool { return list[i].position.y > list[j].position.y })
	maxY := list[0].position.y
	sort.Slice(list, func(i, j int) bool { return list[i].position.x < list[j].position.x })
	minX := list[0].position.x
	sort.Slice(list, func(i, j int) bool { return list[i].position.x > list[j].position.x })
	maxX := list[0].position.x

	diffX = maxX - minX
	diffY = maxY - minY
	return
}

func print(list points) (str string) {
	area := make(areaMap)
	for _, r := range list {
		area[r.position] = true
	}
	sort.Slice(list, func(i, j int) bool { return list[i].position.y < list[j].position.y })
	minY := list[0].position.y
	sort.Slice(list, func(i, j int) bool { return list[i].position.y > list[j].position.y })
	maxY := list[0].position.y
	sort.Slice(list, func(i, j int) bool { return list[i].position.x < list[j].position.x })
	minX := list[0].position.x
	sort.Slice(list, func(i, j int) bool { return list[i].position.x > list[j].position.x })
	maxX := list[0].position.x
	for i := minY; i <= maxY; i++ {
		s := ""
		for n := minX; n <= maxX; n++ {
			p := pair{x: n, y: i}
			_, found := area[p]
			if found {
				s += "#"
			} else {
				s += "."
			}
		}
		str += s + "\n"
	}
	return
}

func main() {
	data, err := adventofcode2017.GetInput("day10.txt")
	if err != nil {
		panic(err)
	}
	input := parseInput(data)
	walkSecond(input)
}

func parseInput(input string) (list points) {
	scan := bufio.NewScanner(strings.NewReader(input))
	for scan.Scan() {
		x, y, moveX, moveY := 0, 0, 0, 0
		fmt.Sscanf(scan.Text(), "position=<%d, %d> velocity=<%d, %d>", &x, &y, &moveX, &moveY)
		list = append(list, newPoint(x, y, moveX, moveY))
	}
	return
}

func walkSecond(list points) {
	diffX := math.MaxInt64
	diffY := math.MaxInt64
	str := ""
	_, _, _ = diffX, diffY, str
	for n := 0; ; n++ {
		for i := 0; i < len(list); i++ {
			list[i] = list[i].move()
		}
		x, y := getMinValues(list)
		if x > diffX && y > diffY {
			for i := 0; i < len(list); i++ {
				list[i] = list[i].back()
			}
			fmt.Println("found it after", n, "sec")
			fmt.Println(print(list))
			break
		}
		if x < diffX {
			diffX = x
		}
		if y < diffY {
			diffY = y
		}

	}
}
