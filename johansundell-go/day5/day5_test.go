package main

import (
	"testing"
)

func Test_Ex1(t *testing.T) {
	str := "aA"
	if parseString(str) != "" {
		t.Fail()
	}
}

func Test_Ex1_e(t *testing.T) {
	str := "Aa"
	if parseString(str) != "" {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if parseString("abBA") != "" {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if parseString("abAB") != "abAB" {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if parseString("aabAAB") != "aabAAB" {
		t.Fail()
	}
}

func Test_Ex5(t *testing.T) {
	if parseString("dabAcCaCBAcCcaDA") != "dabCBAcaDA" {
		t.Fail()
	}
}
