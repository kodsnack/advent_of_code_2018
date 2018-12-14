package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
)

var d = int('a') - int('A')

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	data2, _ := ioutil.ReadFile("input.txt")

	fmt.Printf("Reduced to %d\n", reduce(data))

	for x := int('a'); x <= int('z'); x++ {
		copy(data, data2)
		remove(data, x)
		c := reduce(data)
		fmt.Printf("Reduced to %d with %s and %s removed\n", c, string(x), string(x-d))
	}
}

func remove(data []byte, x int) {
	for i := 0; i < len(data); i++ {
		if data[i] == byte(x) || data[i] == byte(x-d) {
			data[i] = 0
		}
	}
}

func reduce(data []byte) (c int) {
	for {
		last := 0
		for i := 0; i < len(data); i++ {
			if data[i] == 0 {
				continue
			}
			if i > 0 && data[last] > 0 && abs(int(data[i])-int(data[last])) == d {
				data[i] = 0
				data[last] = 0
			}
			last = i
		}
		c2 := len(data) - bytes.Count(data, []byte{0})
		if c2 == c {
			break
		}
		c = c2
	}
	return c
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}
