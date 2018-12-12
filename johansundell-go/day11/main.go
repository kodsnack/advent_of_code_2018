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
	i := ((rackId * c.y) + serial) * rackId
	runes := strings.Split(fmt.Sprintf("%d", i), "")
	i, _ = strconv.Atoi(string(runes[len(runes)-3]))
	return i - 5
}

func main() {
	fmt.Println("sudde")
}
