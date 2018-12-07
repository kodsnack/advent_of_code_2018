package main

import (
	"fmt"
	"strings"
)

type listType map[string]string

func main() {
	fmt.Println("sudde")
}

func parseData(input string) {
	data := strings.Split(input, "\n")
	list := make(listType)
	//	keys := ""
	for _, r := range data {
		str := strings.Fields(r)
		//fmt.Println(str[1], str[7])
		list[str[7]] += str[1]
		list[str[1]] += ""
	}
	fmt.Println(list)
}
