// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"testing"
)

// Point to an input file with an env var
var inputFilename = os.Getenv("TDINPUT")

func TestLexer(t *testing.T) {
	wantNumTokens := 168398
	buf, err := ioutil.ReadFile(inputFilename)

	if err != nil {
		t.Fatal(err)
	}

	toks1 := tokenizeAllPrealloc(buf)
	toks2 := tokenizeAllAppend(buf)
	if len(toks1) != wantNumTokens || len(toks2) != wantNumTokens {
		t.Fatalf("got toks1=%d, toks2=%d; want %d", len(toks1), len(toks2), wantNumTokens)
	}

	for i := range toks1 {
		if toks1[i] != toks2[i] {
			t.Errorf("[%d] %v != %v", i, toks1[i], toks2[i])
		}
	}

	// Find a known token deep in the token slice for relative comparisons
	var foundpos int
	for i, v := range toks1 {
		if v.Val == "VCVTf2xsd" {
			foundpos = i
			break
		}
	}
	if foundpos == 0 {
		t.Error("got foundpos=0, want > 0")
	}

	// Deeper check on some known tokens in this file in known positions
	// val is the token value; if expected val == "", the value of the token is
	// ingored when comparing.
	var tests = []struct {
		index int
		name  TokenName
		val   string
	}{
		{0, COMMENT, ""},
		{10, COMMENT, ""},

		{11, IDENTIFIER, "let"},
		{12, IDENTIFIER, "Component"},
		{13, EQUALS, ""},
		{14, QUOTE, "\"Sema\""},
		{15, IDENTIFIER, "in"},
		{16, L_BRACE, "{"},

		{foundpos + 1, COLON, ":"},
		{foundpos + 2, IDENTIFIER, "N2VCvtD"},
		{foundpos + 3, L_ANG, ""},
		{foundpos + 4, NUMBER, "0"},
		{foundpos + 5, COMMA, ""},
		{foundpos + 6, NUMBER, "1"},
		{foundpos + 17, QUOTE, "\"s32.f32\""},
	}

	for _, tt := range tests {
		testname := fmt.Sprintf("%d", tt.index)
		t.Run(testname, func(t *testing.T) {
			if tt.name != toks1[tt.index].Name {
				t.Errorf("got name %v, want %v", toks1[tt.index].Name, tt.name)
			}
			if tt.val != "" && tt.val != toks1[tt.index].Val {
				t.Errorf("got val %v, want %v", toks1[tt.index].Val, tt.val)
			}
		})
	}
}

func BenchmarkLexerAppend(b *testing.B) {
	buf, err := ioutil.ReadFile(inputFilename)
	if err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		tokenizeAllAppend(buf)
	}
}

func BenchmarkLexerPrealloc(b *testing.B) {
	buf, err := ioutil.ReadFile(inputFilename)
	if err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		tokenizeAllPrealloc(buf)
	}
}
