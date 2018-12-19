package main

import (
	"fmt"
	"time"
)

func main() {
	start := time.Now()
	defer func() {
		fmt.Printf("Done in %v\n", time.Since(start))
	}()

	fmt.Println("part 1:", computeR0(0), "part 2:", computeR0(1))
}

// optimized algo to compute sum of divisors :)
func computeR0(r0 int) int {
	r2 := computeR2(r0)
	// init with 1 + self
	r0 = 1 + r2
	m2 := 1
	// remove 2^x divisors
	for r2&1 == 0 {
		m2 = m2 << 1
		r2 = r2 >> 1
		r0 += (m2 + r2)
	}
	for r1 := 3; r1*r1 < r2; r1 += 2 {
		if r2%r1 == 0 {
			r0 += r1
			r0 += r1 * m2
			r0 += r2 / r1
			r0 += (r2 / r1) * m2
		}
	}
	return r0
}

// original algo
func computeR2(r0 int) int {
	r2 := 2*2*19*11 + 2*22 + 18
	if r0 == 0 {
		return r2
	}
	r2 += 30 * (27*28 + 29) * 14 * 32
	return r2
}
