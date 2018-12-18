package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	// assumes input.txt is sorted already
	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")

	var (
		guard       string
		total       = make(map[string]int)
		minutes     = make(map[string][]int)
		sleepMinute int
		max         int
		maxGuard    string
		d, e        int
	)

	for _, r := range rows {
		f := strings.Fields(r)
		fmt.Sscanf(f[1], "%d:%d]", &d, &e)
		switch f[2] {
		case "Guard":
			guard = f[3]
		case "falls":
			sleepMinute = e
		case "wakes":
			asleepMinutes := e - sleepMinute
			total[guard] += asleepMinutes
			if total[guard] > max {
				max = total[guard]
				maxGuard = guard
			}
			if minutes[guard] == nil {
				minutes[guard] = make([]int, 10*60)
			}
			for i := sleepMinute; i < sleepMinute+asleepMinutes; i++ {
				minutes[guard][i]++
			}
		default:
			panic("Unknown action")
		}
	}

	fmt.Printf("Guard %s was asleep total %d minutes\n", maxGuard, max)
	m, mi := 0, 0
	for i := range minutes[maxGuard] {
		if minutes[maxGuard][i] > m {
			m = minutes[maxGuard][i]
			mi = i
		}
	}
	fmt.Printf("He sleept most at minute %d (%d)\n", mi, m)

	m, mi = 0, 0
	g := ""
	for k, v := range minutes {
		for i := range v {
			if v[i] > m {
				m = v[i]
				mi = i
				g = k
			}
		}
	}
	fmt.Printf("Guard %s was asleep %d times at minute %d\n", g, m, mi)
}
