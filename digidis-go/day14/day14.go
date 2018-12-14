package main

import (
	"bytes"
	"fmt"
	"time"
)

var (
	inputA = 890691
	inputB = []byte{8, 9, 0, 6, 9, 1}
)

func main() {

	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	a, b := 0, 1
	scores := []byte{3, 7}
	lastTarget := inputB[len(inputB)-1]

	doneA, doneB := false, false

	for !doneA || !doneB {
		n := scores[a] + scores[b]
		if n < 10 {
			scores = append(scores, n)
			if n == lastTarget {
				doneB = checkTarget(scores, inputB)
			}
		} else {
			scores = append(scores, 1)
			if lastTarget == 1 {
				doneB = checkTarget(scores, inputB)
			}
			scores = append(scores, n-10)
			if n-10 == lastTarget {
				doneB = checkTarget(scores, inputB)
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

		if len(scores) > inputA+10 {
			doneA = true
		}
	}

	fmt.Println(scores[inputA : inputA+10])
}

func checkTarget(scores, target []byte) bool {
	if len(scores) >= len(target) && bytes.Equal(scores[len(scores)-len(target):], target) {
		fmt.Println("Found target after", len(scores)-len(target))
		return true
	}
	return false
}
