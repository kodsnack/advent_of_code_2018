package main

import (
	"testing"
)

var testData = []string{"abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"}

func Test_Ex1(t *testing.T) {
	if a, b := countStr(testData[0]); a != false && b != false {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	if a, b := countStr(testData[1]); a != true && b != true {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	if a, b := countStr(testData[2]); a != true && b != false {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	if a, b := countStr(testData[3]); a != false && b != true {
		t.Fail()
	}
}

func Test_Ex5(t *testing.T) {
	if a, b := countStr(testData[4]); a != true && b != false {
		t.Fail()
	}
}

func Test_Ex6(t *testing.T) {
	if a, b := countStr(testData[5]); a != true && b != false {
		t.Fail()
	}
}

func Test_Ex7(t *testing.T) {
	if a, b := countStr(testData[6]); a != false && b != true {
		t.Fail()
	}
}

func Test_Ex8(t *testing.T) {
	if a := makeChecksum(testData); a != 12 {
		t.Fail()
	}
}
