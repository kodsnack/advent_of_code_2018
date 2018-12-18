package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	data, _ := ioutil.ReadFile("input.txt")
	rows := strings.Split(string(data), "\n")

	var have2, have3 int
	for _, r := range rows {
		count := make(map[byte]int)
		for i := 0; i < len(r); i++ {
			count[r[i]]++
		}
		for _, c := range count {
			if c == 2 {
				have2++
				break
			}
		}
		for _, c := range count {
			if c == 3 {
				have3++
				break
			}
		}
	}
	fmt.Println(have2 * have3)

	for _, r := range rows {
		for _, r2 := range rows {
			var diff int
			for i := 0; i < len(r); i++ {
				if r[i] != r2[i] {
					diff++
				}
			}
			if diff == 1 {
				fmt.Println(r)
				fmt.Println(r2)
				return
			}
		}
	}

}
