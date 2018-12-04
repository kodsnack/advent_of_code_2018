package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type logEntry struct {
	t    time.Time
	i    int64
	mess string
}

type guard struct {
	id       string
	duration int
	sleepLog map[int]int
}

var guards map[string]guard

/*
var guards [guard]bool
*/
func init() {
	guards = make(map[string]guard)
}

type logType []logEntry

func main() {
	data, err := adventofcode2017.GetInput("day4.txt")
	if err != nil {
		panic(err)
	}
	parseData(strings.Split(data, "\n"))
}

func parseData(input []string) {
	//fmt.Println(len(input))
	//str := strings.FieldsFunc(input[1], func(r rune) bool { return r == '[' || r == ']' })
	//str := strings.Fields(input[1])
	//fmt.Println(time.Parse("2006-01-02 15:04", str[0]))
	/*for _, s := range str {
		fmt.Println(s)
	}*/
	log := logType{}
	for _, s := range input {
		str := strings.FieldsFunc(s, func(r rune) bool { return r == '[' || r == ']' })
		g := logEntry{}
		g.t, _ = time.Parse("2006-01-02 15:04", str[0])
		g.i = g.t.Unix()
		//fmt.Println(g.t.String())
		g.mess = strings.TrimSpace(str[1])
		log = append(log, g)
	}
	fmt.Println("Part 2")
	sort.Slice(log, func(i, j int) bool {
		return log[i].i < log[j].i
	})
	//guards := guards{}
	currentGuard := ""
	feelAsleepAt := time.Time{}
	for _, l := range log {
		//fmt.Println(time.Unix(l.i, 0), l.mess)
		str := strings.Fields(l.mess)
		//fmt.Println(str)
		switch {
		case str[0] == "Guard":
			currentGuard = str[1]
		case str[0] == "falls":
			feelAsleepAt = time.Unix(l.i, 0)
		case str[0] == "wakes":
			if _, ok := guards[currentGuard]; !ok {
				g := guard{}
				g.sleepLog = make(map[int]int)
				guards[currentGuard] = g
			}
			g := guards[currentGuard]
			fmt.Println(currentGuard, feelAsleepAt, time.Unix(l.i, 0), "d", feelAsleepAt.Sub(time.Unix(l.i, 0)).Minutes())
			g.duration += int(time.Unix(l.i, 0).Sub(feelAsleepAt).Minutes())

			// TODO: remove this
			g.id = currentGuard
			for i := feelAsleepAt.Minute(); i < time.Unix(l.i, 0).Minute(); i++ {
				g.sleepLog[i]++
			}

			guards[currentGuard] = g
		}

	}

	// TODO: Fix thsi better
	g := guard{}
	top := 0
	for _, r := range guards {
		if r.duration > top {
			g = r
			top = r.duration
		}
	}
	fmt.Println(g)
	top = 0
	n := 0
	for i := 0; i <= 59; i++ {
		if g.sleepLog[i] > top {
			top = g.sleepLog[i]
			n = i
		}
	}
	fmt.Println(getIntFromId(g.id) * n)

	str := ""
	top = 0
	n = 0
	for k, r := range guards {
		for i := 0; i <= 59; i++ {
			if r.sleepLog[i] > top {
				str = k
				top = r.sleepLog[i]
				n = i
			}
		}
	}
	fmt.Println(getIntFromId(str) * n)
}

func getIntFromId(str string) int {
	str = strings.Replace(str, "#", "", -1)
	s, _ := strconv.Atoi(str)
	return s
}
