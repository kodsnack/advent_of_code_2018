package main

import (
	"testing"
)

func Test_Ex1(t *testing.T) {
	if n := playGame(9, 25); n != 32 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if n := playGame(10, 1618); n != 8317 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if n := playGame(13, 7999); n != 146373 {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if n := playGame(17, 1104); n != 2764 {
		t.Fail()
	}
}

func Test_Ex5(t *testing.T) {
	if n := playGame(21, 6111); n != 54718 {
		t.Fail()
	}
}

func Test_Ex6(t *testing.T) {
	if n := playGame(30, 5807); n != 37305 {
		t.Fail()
	}
}
