package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strings"
)

var before = make(map[string][]string)

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")
	for _, r := range rows {
		f := strings.Fields(r)
		before[f[7]] = append(before[f[7]], f[1])
		before[f[1]] = append(before[f[1]], []string{}...)
	}

	part1()
	part2()
}

func part1() {
	sequence := ""
	done := make(map[string]bool)
	for {
		c := []string{}
		for k, v := range before {
			if done[k] {
				continue
			}
			all := true
			for _, b := range v {
				if !done[b] {
					all = false
					break
				}
			}
			if all {
				c = append(c, k)
			}
		}
		if len(c) == 0 {
			fmt.Printf("Nothing left to at %s\n", sequence)
			return
		}
		sort.Sort(sort.StringSlice(c))
		done[c[0]] = true
		sequence += c[0]
	}
}

func part2() {
	sequence := ""
	done := make(map[string]bool)
	inprogress := make(map[string]bool)
	workers := make([]int, 5)
	objs := make([]string, 5)

	s := 0
	for {
		for w := range workers {
			if workers[w] <= s && objs[w] != "" {
				delete(inprogress, objs[w])
				done[objs[w]] = true
				sequence += objs[w]
				objs[w] = ""
			}
		}
		for w := range workers {
			if workers[w] <= s {
				c := []string{}
				for k, v := range before {
					if done[k] || inprogress[k] {
						continue
					}
					all := true
					for _, b := range v {
						if !done[b] {
							all = false
							break
						}
					}
					if all {
						c = append(c, k)
					}
				}
				if len(c) == 0 && len(inprogress) == 0 {
					fmt.Printf("All done at second %d : %s\n", s, sequence)
					return
				}
				if len(c) > 0 {
					sort.Sort(sort.StringSlice(c))
					time := 60 + 1 + int(c[0][0]-'A')
					fmt.Printf("[%d] Worker %d picking %v out of %v (%v), it will take %d sec and be done at %d\n", s, w, c[0], c, sequence, time, s+time)
					inprogress[c[0]] = true
					workers[w] = s + time
					objs[w] = c[0]
				}
			}
		}
		s++
	}
}
