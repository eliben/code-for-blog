// Binary tree construction from inorder-depth representation.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import "testing"

func TestParse(t *testing.T) {
	var tests = []struct {
		input string
		want  string
	}{
		{"[]", ""},
		{"[1,2]", "(1 1) (2 1)"},
		{"[1,[2,8]]", "(1 1) (2 2) (8 2)"},
		{"[[1,9],2]", "(1 2) (9 2) (2 1)"},
		{"[[4,8],[3,5]]", "(4 2) (8 2) (3 2) (5 2)"},
		{"[[6,9],[[3,4],2]]", "(6 2) (9 2) (3 3) (4 3) (2 2)"},
		{"[[1,2],[[3,4],5]]", "(1 2) (2 2) (3 3) (4 3) (5 2)"},
		{"[[[[1,2],3],4],5]", "(1 4) (2 4) (3 3) (4 2) (5 1)"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			ans := Parse(tt.input).String()
			if ans != tt.want {
				t.Errorf("got %v, want %v", ans, tt.want)
			}
		})
	}
}

func TestBuildTree(t *testing.T) {
	var tests = []struct {
		input string
		want  string
	}{
		{"[]", "()"},
		{"[1,2]", "(1 2)"},
		{"[1,[2,8]]", "(1 (2 8))"},
		{"[[4,8],3]", "((4 8) 3)"},
		{"[[4,8],[3,5]]", "((4 8) (3 5))"},
		{"[[6,9],[[3,4],2]]", "((6 9) ((3 4) 2))"},
		{"[[[[1,2],3],4],5]", "((((1 2) 3) 4) 5)"},
		{"[7,[5,[4,[9,1]]]]", "(7 (5 (4 (9 1))))"},
		{"[[[4,8],[3,5]], [[3,7],[1,9]]]", "(((4 8) (3 5)) ((3 7) (1 9)))"},
		{"[6,[[[4,8],[3,5]], [[3,7],[1,9]]]]", "(6 (((4 8) (3 5)) ((3 7) (1 9))))"},
		{"[[[[4,8],[3,5]], [[3,7],[1,9]]],6]", "((((4 8) (3 5)) ((3 7) (1 9))) 6)"},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			dl := Parse(tt.input)
			ansRec := dl.BuildTreeRec()
			ansRec.Verify()
			if ansRec.String() != tt.want {
				t.Errorf("rec got %v, want %v", ansRec.String(), tt.want)
			}

			ansIter := dl.BuildTree()
			ansIter.Verify()
			if ansIter.String() != tt.want {
				t.Errorf("iter got %v, want %v", ansIter.String(), tt.want)
			}
		})
	}
}
