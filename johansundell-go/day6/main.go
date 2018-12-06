package main

import (
	"fmt"
	"math"
	"sort"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type point struct {
	y, x, id, sum int
	isEdge        bool
}

func (a *point) distance(b point) int {
	return int(math.Abs(float64(a.x)-float64(b.x))) + int(math.Abs(float64(a.y)-float64(b.y)))
}

type points []point

func main() {
	data, err := adventofcode2017.GetInput("day6.txt")
	if err != nil {
		panic(err)
	}
	parseInput(data)
}

func parseInput(input string) {

	res := strings.Fields(input)
	mp := make(points, len(res)/2)
	for i, n := 0, 0; i < len(res); i = i + 2 {
		y, _ := strconv.Atoi(strings.TrimSpace(strings.Replace(res[i], ",", "", -1)))
		x, _ := strconv.Atoi(strings.TrimSpace(strings.Replace(res[i+1], ",", "", -1)))
		mp[n] = point{y: y, x: x, id: n}
		n++
	}
	maxY, maxX := getMaxYx(mp)
	//fmt.Println(mp)
	// Left
	for y := 0; y < maxY; y++ {
		p := point{y: y, x: 0}
		sort.Slice(mp, func(i, j int) bool { return mp[i].distance(p) < mp[j].distance(p) })
		mp[0].isEdge = true
	}
	// Right
	for y := 0; y < maxY; y++ {
		p := point{y: y, x: maxX}
		sort.Slice(mp, func(i, j int) bool { return mp[i].distance(p) < mp[j].distance(p) })
		mp[0].isEdge = true
	}
	// Top
	for x := 0; x < maxX; x++ {
		p := point{y: 0, x: x}
		sort.Slice(mp, func(i, j int) bool { return mp[i].distance(p) < mp[j].distance(p) })
		mp[0].isEdge = true
	}
	// Bottom
	for x := 0; x < maxX; x++ {
		p := point{y: maxY, x: x}
		sort.Slice(mp, func(i, j int) bool { return mp[i].distance(p) < mp[j].distance(p) })
		mp[0].isEdge = true
	}
	//fmt.Println(mp)

	//dist := 0
	for y := 0; y < maxY; y++ {
		//str := ""
		for x := 0; x < maxX; x++ {
			p := point{y: y, x: x}
			//t := point{y: y, x: x}
			sort.Slice(mp, func(i, j int) bool { return mp[i].distance(p) < mp[j].distance(p) })
			/*if mp[0].distance(p) == mp[1].distance(p) {
				str += "."
			} else {
				str += fmt.Sprintf("%d", mp[0].id)
			}*/

			if !mp[0].isEdge && mp[0].distance(p) != mp[1].distance(p) {
				mp[0].sum++
			}
			/*
				if mp[0].id == 3 && mp[0].distance(p) != mp[1].distance(p) { //&& mp[0].distance(p) != mp[1].distance(p)
					dist++
					//fmt.Println(p)
				}
			*/
		}
		//fmt.Println(str)
	}
	//fmt.Println(dist)
	top := 0
	for i := 0; i < len(mp); i++ {
		if !mp[i].isEdge && mp[i].sum > top {
			top = mp[i].sum
		}
	}
	fmt.Println(top)
}

func getMaxYx(input []point) (y, x int) {
	sort.Slice(input, func(i, j int) bool {
		return input[i].y > input[j].y
	})
	y = input[0].y
	sort.Slice(input, func(i, j int) bool {
		return input[i].x > input[j].x
	})
	x = input[0].x
	return
}
