package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	var (
		v, step int
		counts  = make(map[int]bool)
	)
	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")
	for {
		for _, r := range rows {
			if r[0] == '+' {
				v += atoi(r[1:])
			} else {
				v -= atoi(r[1:])
			}
			if counts[v] {
				fmt.Println(v)
				return
			}
			counts[v] = true
		}
		if step == 0 {
			fmt.Println(v)
		}
		step++
	}
}

func atoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}
