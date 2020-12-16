package main

import "testing"

func TestAboot(t *testing.T) {
	if Aboot() != "boo" {
		t.Fatal("want boo")
	}
}
