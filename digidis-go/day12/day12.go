package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"
	"time"
)

func main() {

	var (
		rules = make(map[string]string)
		state []byte
		dots  = []byte(".....")
	)

	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")
	fmt.Sscanf(rows[0], "initial state: %s", &state)
	fmt.Printf("%s\n", state)
	for i := 2; i < len(rows); i++ {
		f := strings.Split(rows[i], " => ")
		rules[f[0]] = f[1]
	}

	var (
		zero, lastsum, lastDiff, sameDiff int
		N                                 = 50000000000
	)
	for g := 1; g <= N; g++ {

		if !bytes.HasPrefix(state, dots) {
			state = append(dots, state...)
			zero += len(dots)
		}

		if !bytes.HasSuffix(state, dots) {
			state = append(state, dots...)
		}

		next := make([]byte, len(state))
		copy(next, state)

		for p := 2; p < len(state)-2; p++ {
			found := false
			for k, v := range rules {
				if string(state[p-2:p+3]) == k {
					next[p] = v[0]
					found = true
					break
				}
			}
			if !found {
				panic("No rule matched")
			}
		}
		state = next

		sum := 0
		for p := 0; p < len(state); p++ {
			if state[p] == '#' {
				sum += (p - zero)
			}
		}

		diff := sum - lastsum
		if diff == lastDiff {
			sameDiff++
		}
		lastsum, lastDiff = sum, diff

		fmt.Printf("Sum: %d at %d, growed by %d\n", sum, g, diff)

		if sameDiff > 10 {
			fmt.Printf("Diff stable at generation %v\n", g)
			fmt.Printf("Sum at %d is %d\n", N, sum+(N-g)*diff)
			return
		}

	}
}
