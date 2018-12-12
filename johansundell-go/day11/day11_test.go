package main

import (
	"testing"
)

func Test_Ext1(t *testing.T) {
	if i := (cell{x: 3, y: 5}).power(8); i != 4 {
		//fmt.Println(i)
		t.Fail()
	}
}

func Test_Ext2(t *testing.T) {
	if i := (cell{x: 122, y: 79}).power(57); i != -5 {
		//fmt.Println(i)
		t.Fail()
	}
}

func Test_Ext3(t *testing.T) {
	if i := (cell{x: 217, y: 196}).power(39); i != -0 {
		//fmt.Println(i)
		t.Fail()
	}
}

func Test_Ext4(t *testing.T) {
	if i := (cell{x: 101, y: 153}).power(71); i != 4 {
		//fmt.Println(i)
		t.Fail()
	}
}
