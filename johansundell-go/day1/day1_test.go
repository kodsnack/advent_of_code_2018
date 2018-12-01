package main

import "testing"

func Test_Ex1(t *testing.T) {
	if makeInts("+1, +1, +1", ",") != 3 {
		t.Fail()
	}
	str := `
	+1
	+1 
	 +1
	`
	if makeInts(str, "\n") != 3 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if makeInts("+1, +1, -2", ",") != 0 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if makeInts("-1, -2, -3", ",") != -6 {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if findDup("+1, -1", ",") != 0 {
		t.Fail()
	}
}

func Test_Ex5(t *testing.T) {
	if findDup("+3, +3, +4, -2, -4", ",") != 10 {
		t.Fail()
	}
}

func Test_Ex6(t *testing.T) {
	if findDup("-6, +3, +8, +5, -6", ",") != 5 {
		t.Fail()
	}
}

func Test_Ex7(t *testing.T) {
	if findDup("+7, +7, -2, -7, -4", ",") != 14 {
		t.Fail()
	}
}
