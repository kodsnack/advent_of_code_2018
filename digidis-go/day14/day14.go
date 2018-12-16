package main

import (
	"fmt"
	"time"
)

var (
	inputA = 890691
	inputB = []byte{8, 9, 0, 6, 9, 1}
	scores = make([]byte, 2, 30000000)
)

func main() {

	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	a, b := 0, 1
	scores[0] = 3
	scores[1] = 7
	match := 0

	// This assumes that pattern inputB is found after inputA digits
	for {
		n := scores[a] + scores[b]
		if n < 10 {
			scores = append(scores, n)
			if n == inputB[match] {
				match++
				if match == len(inputB) {
					fmt.Println("Found target after", len(scores)-len(inputB))
					break
				}
			} else {
				match = 0
			}
		} else {
			scores = append(scores, 1)
			if 1 == inputB[match] {
				match++
				if match == len(inputB) {
					fmt.Println("Found target after", len(scores)-len(inputB))
					break
				}
			} else {
				match = 0
			}

			scores = append(scores, n-10)
			if n == inputB[match] {
				match++
				if match == len(inputB) {
					fmt.Println("Found target after", len(scores)-len(inputB))
					break
				}
			} else {
				match = 0
			}
		}

		a += int(scores[a]) + 1
		if a >= len(scores) {
			a = a % len(scores)
		}
		b += int(scores[b]) + 1
		if b >= len(scores) {
			b = b % len(scores)
		}

	}
	fmt.Println(scores[inputA : inputA+10])

}
