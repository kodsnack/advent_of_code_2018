package main

import (
	"fmt"
	"sort"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type listType map[string]string

type instructionType struct {
	key string
	str string
}

type worker struct {
}

func (i *instructionType) removeS(s string) string {
	return strings.Replace(i.str, s, "", -1)
}

type instructionsType []instructionType

func main() {
	data, err := adventofcode2017.GetInput("day7.txt")
	if err != nil {
		panic(err)
	}
	fmt.Println(parseData(data))
}

func parseData(input string) (part1 string) {
	data := strings.Split(input, "\n")
	list := make(listType)
	for _, r := range data {
		str := strings.Fields(r)
		list[str[7]] += str[1]
		list[str[1]] += ""
	}

	instructions := make(instructionsType, 0)
	for k, r := range list {
		instructions = append(instructions, instructionType{key: k, str: r})
	}

	for i := 0; len(instructions) > 0; i++ {
		sort.Slice(instructions, func(i, j int) bool {
			if len(instructions[i].str) == len(instructions[j].str) {
				return instructions[i].key < instructions[j].key
			}
			return len(instructions[i].str) < len(instructions[j].str)
		})
		key := instructions[0].key
		part1 += key
		for n, r := range instructions {
			instructions[n].str = r.removeS(key)
		}
		instructions = append(instructions[:0], instructions[1:]...)
	}
	return
}
