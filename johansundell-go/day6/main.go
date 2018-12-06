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

func (a *point) sumDist(mp points) (sum int) {
	for _, r := range mp {
		sum += a.distance(r)
	}
	return
}

type points []point

func main() {
	data, err := adventofcode2017.GetInput("day6.txt")
	if err != nil {
		panic(err)
	}
	fmt.Println(parseInput(data, 10000))
}

func parseInput(input string, part2Limit int) (part1, part2 int) {
	res := strings.Fields(input)
	mp := make(points, len(res)/2)
	for i, n := 0, 0; i < len(res); i = i + 2 {
		y, _ := strconv.Atoi(strings.TrimSpace(strings.Replace(res[i], ",", "", -1)))
		x, _ := strconv.Atoi(strings.TrimSpace(strings.Replace(res[i+1], ",", "", -1)))
		mp[n] = point{y: y, x: x, id: n}
		n++
	}
	maxY, maxX := getMaxYx(mp)

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

	for y := 0; y < maxY; y++ {
		for x := 0; x < maxX; x++ {
			p := point{y: y, x: x}
			if p.sumDist(mp) < part2Limit {
				part2++
			}
			sort.Slice(mp, func(i, j int) bool { return mp[i].distance(p) < mp[j].distance(p) })
			if !mp[0].isEdge && mp[0].distance(p) != mp[1].distance(p) {
				mp[0].sum++
			}
		}
	}

	for i := 0; i < len(mp); i++ {
		if !mp[i].isEdge && mp[i].sum > part1 {
			part1 = mp[i].sum
		}
	}
	return
}

func getMaxYx(input []point) (y, x int) {
	sort.Slice(input, func(i, j int) bool { return input[i].y > input[j].y })
	y = input[0].y
	sort.Slice(input, func(i, j int) bool { return input[i].x > input[j].x })
	x = input[0].x
	return
}
