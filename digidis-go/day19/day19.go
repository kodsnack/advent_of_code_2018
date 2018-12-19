package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"time"
)

var (
	reg     = []int{0, 0, 0, 0, 0, 0, 0}
	code    [][]int
	ipr     int
	opcodes = make(map[int]string)
)

func main() {
	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")

	for _, r := range rows {
		c := strings.Fields(r)
		if c[0] == "#ip" {
			ipr = atoi(c[1])
			continue
		}
		code = append(code, []int{
			instructions[c[0]], atoi(c[1]), atoi(c[2]), atoi(c[3]),
		})
	}

	v, o := run(0)
	fmt.Printf("Part 1: %v (%d operations)\n", v, o)

	v, o = run(1)
	fmt.Printf("Part 2: %v (%d operations)\n", v, o)
}

func run(r0 int) (int, int) {
	reg = []int{r0, 0, 0, 0, 0, 0, 0}
	ip := 0
	ops := 0
	for {
		reg[ipr] = ip
		eval(code[ip])
		ip = reg[ipr] + 1
		if ip >= len(code) {
			break
		}
		ops++
	}
	return reg[0], ops
}

var instructions = map[string]int{
	"addr": 0, "addi": 1, "mulr": 2, "muli": 3,
	"banr": 4, "bani": 5, "borr": 6, "bori": 7,
	"setr": 8, "seti": 9, "gtir": 10, "gtri": 11,
	"gtrr": 12, "eqir": 13, "eqri": 14, "eqrr": 15,
}

func eval(code []int) {
	switch code[0] {
	case 0:
		reg[code[3]] = reg[code[1]] + reg[code[2]]
	case 1:
		reg[code[3]] = reg[code[1]] + code[2]
	case 2:
		reg[code[3]] = reg[code[1]] * reg[code[2]]
	case 3:
		reg[code[3]] = reg[code[1]] * code[2]
	case 4:
		reg[code[3]] = reg[code[1]] & reg[code[2]]
	case 5:
		reg[code[3]] = reg[code[1]] & code[2]
	case 6:
		reg[code[3]] = reg[code[1]] | reg[code[2]]
	case 7:
		reg[code[3]] = reg[code[1]] | code[2]
	case 8:
		reg[code[3]] = reg[code[1]]
	case 9:
		reg[code[3]] = code[1]
	case 10:
		if code[1] > reg[code[2]] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case 11:
		if reg[code[1]] > code[2] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case 12:
		if reg[code[1]] > reg[code[2]] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case 13:
		if code[1] == reg[code[2]] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case 14:
		if reg[code[1]] == code[2] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case 15:
		if reg[code[1]] == reg[code[2]] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	}
}

func atoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}
