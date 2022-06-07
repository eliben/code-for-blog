// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"testing"
)

func TestSmokeTest(t *testing.T) {
	input := " + { =  ."
	toks := tokenizeAllAppend(input)

	wantToks := []Token{
		Token{PLUS, "+", 1},
		Token{L_BRACE, "{", 3},
		Token{EQUALS, "=", 5},
		Token{PERIOD, ".", 8},
		Token{EOF, "", 9},
	}

	if !tokSliceEqual(toks, wantToks) {
		t.Errorf("got\n\t%+v\nexpected\n\t%+v", toks, wantToks)
	}
}

func TestLexer(t *testing.T) {
	// Helper functions for easy creation of expected tokens with and without
	// position.
	tok := func(name TokenName, val string) Token {
		return Token{name, val, 0}
	}

	tokp := func(name TokenName, val string, pos int) Token {
		return Token{name, val, pos}
	}

	var tests = []struct {
		input    string
		wantToks []Token
	}{
		{" , # ", []Token{
			tok(COMMA, ","),
			tok(POUND, "#"),
			tok(EOF, ""),
		}},
		{" , # ", []Token{
			tokp(COMMA, ",", 1),
			tokp(POUND, "#", 3),
			tokp(EOF, "", 5),
		}},
		{"/ *", []Token{
			tok(DIVIDE, "/"),
			tok(MULTIPLY, "*"),
			tokp(EOF, "", 3),
		}},
		{" bob + alice", []Token{
			tokp(IDENTIFIER, "bob", 1),
			tokp(PLUS, "+", 5),
			tokp(IDENTIFIER, "alice", 7),
			tokp(EOF, "", 12),
		}},
		{"_umami9", []Token{
			tok(IDENTIFIER, "_umami9"),
			tok(EOF, ""),
		}},
		{"9 10 3033 0", []Token{
			tok(NUMBER, "9"),
			tok(NUMBER, "10"),
			tok(NUMBER, "3033"),
			tok(NUMBER, "0"),
			tok(EOF, ""),
		}},
		{`let name = "日本語"`, []Token{
			tok(IDENTIFIER, "let"),
			tok(IDENTIFIER, "name"),
			tok(EQUALS, "="),
			tok(QUOTE, `"日本語"`),
			tok(EOF, ""),
		}},
		{`jon + "stringjon"`, []Token{
			tok(IDENTIFIER, "jon"),
			tok(PLUS, "+"),
			tok(QUOTE, `"stringjon"`),
			tok(EOF, ""),
		}},
		{` "str on" / tok`, []Token{
			tokp(QUOTE, `"str on"`, 1),
			tokp(DIVIDE, "/", 10),
			tokp(IDENTIFIER, "tok", 12),
			tokp(EOF, "", 15),
		}},
		{"joe // cmt1\n  // cmt2\n200// cmt 9", []Token{
			tok(IDENTIFIER, "joe"),
			tok(COMMENT, "// cmt1"),
			tok(COMMENT, "// cmt2"),
			tok(NUMBER, "200"),
			tok(COMMENT, "// cmt 9"),
			tok(EOF, ""),
		}},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			gotToks := tokenizeAllAppend(test.input)
			if !tokSliceEqual(gotToks, test.wantToks) {
				t.Errorf("got\n\t%+v\nexpected\n\t%+v", gotToks, test.wantToks)
			}
		})
	}
}

// Point to an input file with an env var
var inputFilename = os.Getenv("TDINPUT")

func TestLexerLargeFile(t *testing.T) {
	wantNumTokens := 168398
	buf, err := ioutil.ReadFile(inputFilename)
	if err != nil {
		t.Fatal(err)
	}
	sbuf := string(buf)
	toks1 := tokenizeAllPrealloc(sbuf)
	toks2 := tokenizeAllAppend(sbuf)
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

// tokSliceEqual compares two slices of tokens for equality; if the position
// of a token is 0 (in either s1 or s2), the positions are not compared.
func tokSliceEqual(s1, s2 []Token) bool {
	if len(s1) != len(s2) {
		return false
	}
	for i := range s1 {
		if s1[i].Name != s2[i].Name || s1[i].Val != s2[i].Val {
			return false
		}
		if s1[i].Pos != 0 && s2[i].Pos != 0 && s1[i].Pos != s2[i].Pos {
			return false
		}
	}
	return true
}

func BenchmarkLexerAppend(b *testing.B) {
	buf, err := ioutil.ReadFile(inputFilename)
	if err != nil {
		b.Fatal(err)
	}
	sbuf := string(buf)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		tokenizeAllAppend(sbuf)
	}
}

func BenchmarkLexerPrealloc(b *testing.B) {
	buf, err := ioutil.ReadFile(inputFilename)
	if err != nil {
		b.Fatal(err)
	}
	sbuf := string(buf)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		tokenizeAllPrealloc(sbuf)
	}
}
