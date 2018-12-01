package main

import "testing"

func Test_Ex1(t *testing.T) {
	if _, n := findDup("+1, -1", ","); n != 0 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if _, n := findDup("+3, +3, +4, -2, -4", ","); n != 10 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if _, n := findDup("-6, +3, +8, +5, -6", ","); n != 5 {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if _, n := findDup("+7, +7, -2, -7, -4", ","); n != 14 {
		t.Fail()
	}
}
