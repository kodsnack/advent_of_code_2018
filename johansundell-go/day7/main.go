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

func (i *instructionType) removeS(s string) string {
	return strings.Replace(i.str, s, "", -1)
}

type instructionsType []instructionType

/*
func (i instructionsType) removeS(s string) (instructionsType) {
	in :=
	for _, r := range i {
		r.removeS(s)
	}
}*/

func main() {
	data, err := adventofcode2017.GetInput("day7.txt")
	if err != nil {
		panic(err)
	}
	fmt.Println(parseData(data))
}

func parseData(input string) string {
	data := strings.Split(input, "\n")
	list := make(listType)
	//	keys := ""
	for _, r := range data {
		str := strings.Fields(r)
		//fmt.Println(str[1], str[7])
		list[str[7]] += str[1]
		list[str[1]] += ""
	}

	//fmt.Println(list)
	instructions := make(instructionsType, 0)
	for k, r := range list {
		i := instructionType{key: k, str: r}
		instructions = append(instructions, i)
	}
	//fmt.Println(instructions)
	res := ""
	for i := 0; len(instructions) > 0; i++ {
		sort.Slice(instructions, func(i, j int) bool {
			if len(instructions[i].str) == len(instructions[j].str) {
				return instructions[i].key < instructions[j].key
			}
			return len(instructions[i].str) < len(instructions[j].str)
		})
		key := instructions[0].key
		res += key
		for n, r := range instructions {
			instructions[n].str = r.removeS(key)
		}
		instructions = append(instructions[:0], instructions[1:]...)
		//fmt.Println(instructions, res)
	}
	return res
}
