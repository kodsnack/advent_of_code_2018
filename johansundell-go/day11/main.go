package main

import (
	"fmt"
	"strconv"
	"strings"
)

type cell struct {
	x, y int
}

func (c cell) power(serial int) int {
	rackId := c.x + 10
	i := rackId * c.y
	i += serial
	//fmt.Println(i)
	i = i * rackId
	//fmt.Println(i)
	runes := strings.Split(fmt.Sprintf("%d", i), "")
	//fmt.Println(runes)
	//fmt.Println(runes[len(runes)-3])
	i, _ = strconv.Atoi(string(runes[len(runes)-3]))
	return i - 5
}

func main() {
	fmt.Println("sudde")
}
