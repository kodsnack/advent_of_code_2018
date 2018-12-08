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
	//isWorking bool
	//w worker
}

type worker struct {
	workingOn string
	secLeft   int
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
	fmt.Println(parseData(data, 60))
}

func parseData(input string, secToAdd int) (part1 string, part2 int) {
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
	//fmt.Printf("%+v", instructions)

	for len(instructions) > 0 {
		sort.Slice(instructions, func(i, j int) bool {
			if len(instructions[i].str) == len(instructions[j].str) {
				return instructions[i].key < instructions[j].key
			}
			return len(instructions[i].str) < len(instructions[j].str)
		})
		key := instructions[0].key
		part1 += key
		//fmt.Println(instructions)
		for n, r := range instructions {
			instructions[n].str = r.removeS(key)
		}

		instructions = append(instructions[:0], instructions[1:]...)
	}

	// Part 2, this is messy ;)
	iList := make(instructionsType, 0)
	for k, r := range list {
		iList = append(iList, instructionType{key: k, str: r})
	}

	wPool := [5]worker{}
	part2str := ""

	for s := 0; true; s++ {
		active := 0
		for i := 0; i < len(wPool); i++ {
			if wPool[i].workingOn != "" {
				wPool[i].secLeft--
				active++
				if wPool[i].secLeft == 0 {
					part2str += wPool[i].workingOn
					s := wPool[i].workingOn
					for n, r := range iList {
						iList[n].str = r.removeS(s)
					}
					wPool[i].workingOn = ""
				}
			}
		}

		sort.Slice(iList, func(i, j int) bool {
			if len(iList[i].str) == len(iList[j].str) {
				return iList[i].key < iList[j].key
			}
			return len(iList[i].str) < len(iList[j].str)
		})

		for i := 0; i < len(wPool) && len(iList) > 0; i++ {
			if wPool[i].workingOn == "" && len(iList[0].str) == 0 {
				wPool[i].workingOn = iList[0].key
				wPool[i].secLeft = int(iList[0].key[0]) - int('A') + 1 + secToAdd
				iList = append(iList[:0], iList[1:]...)
				active++
			}
		}

		if active == 0 {
			part2 = s - 1
			return
		}
	}
	return
}
