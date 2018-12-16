package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"time"
)

var (
	reg          = []int{0, 0, 0, 0}
	opcodes      = make(map[int]string)
	instructions = []string{"addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"}
)

func main() {
	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	do(true)
	do(false)

}

func do(part1 bool) {
	data, _ := ioutil.ReadFile("input-a.txt")
	rows := strings.Split(string(data), "\n")
	var before, code, after []int
	more3 := 0
	done := make(map[string]bool)

	// assuming one loop is enough for part2
	for _, r := range rows {

		if len(r) == 0 {
			m := 0
			var match []string
			for _, instr := range instructions {
				if !part1 && done[instr] {
					continue
				}
				copy(reg, before)
				run(instr, code)
				if eq(reg, after) {
					m++
					match = append(match, instr)
				}
			}
			if m == 1 {
				opcodes[code[0]] = match[0]
				done[match[0]] = true
			}
			if m >= 3 {
				more3++
			}
			before = nil
			after = nil

			continue
		}

		switch r[0] {
		case 'B':
			before = atois(r[9 : len(r)-1])
		case 'A':
			after = atois(r[9 : len(r)-1])
		default:
			code = atois(r)
		}

	}
	if part1 {
		fmt.Printf("More than 3 : %v\n", more3)
		return
	}

	reg = make([]int, 4)
	data, _ = ioutil.ReadFile("input-b.txt")
	rows = strings.Split(string(data), "\n")
	for _, r := range rows {
		code := atois(r)
		run(opcodes[code[0]], code)
	}
	fmt.Printf("Final registers %v\n", reg)
}

func run(op string, code []int) {
	switch op {
	case "addr":
		reg[code[3]] = reg[code[1]] + reg[code[2]]
	case "addi":
		reg[code[3]] = reg[code[1]] + code[2]
	case "mulr":
		reg[code[3]] = reg[code[1]] * reg[code[2]]
	case "muli":
		reg[code[3]] = reg[code[1]] * code[2]
	case "banr":
		reg[code[3]] = reg[code[1]] & reg[code[2]]
	case "bani":
		reg[code[3]] = reg[code[1]] & code[2]
	case "borr":
		reg[code[3]] = reg[code[1]] | reg[code[2]]
	case "bori":
		reg[code[3]] = reg[code[1]] | code[2]
	case "setr":
		reg[code[3]] = reg[code[1]]
	case "seti":
		reg[code[3]] = code[1]
	case "gtir":
		if code[1] > reg[code[2]] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case "gtri":
		if reg[code[1]] > code[2] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case "gtrr":
		if reg[code[1]] > reg[code[2]] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case "eqir":
		if code[1] == reg[code[2]] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case "eqri":
		if reg[code[1]] == code[2] {
			reg[code[3]] = 1
		} else {
			reg[code[3]] = 0
		}
	case "eqrr":
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

func atois(s string) (r []int) {
	s = strings.Replace(s, ",", "", -1)
	f := strings.Fields(s)
	for _, k := range f {
		r = append(r, atoi(k))
	}
	return
}

func eq(a []int, b []int) bool {
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
