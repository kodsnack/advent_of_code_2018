package main

import (
	"testing"
)

var data = []string{"#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"}

func Test_Conflict(t *testing.T) {
	c := claims{}
	for _, d := range data {
		c = append(c, parseClaim(d))
	}
	if !c[0].hasConflict() {
		t.Fail()
	}
	if !c[1].hasConflict() {
		t.Fail()
	}
	if c[2].hasConflict() {
		t.Fail()
	}

	if findThem() != 4 {
		t.Fail()
	}
}
