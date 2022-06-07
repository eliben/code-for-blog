// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"strings"
	"testing"
)

type lexTest struct {
	name  string
	input string
	items []item
}

var (
	tEOF   = item{itemEOF, ""}
	tLeft  = item{itemLeftMeta, "{{"}
	tRight = item{itemRightMeta, "}}"}
	tIf    = item{itemIf, "if"}
	tElse  = item{itemElse, "else"}
	tRange = item{itemRange, "range"}
	tPipe  = item{itemPipe, "|"}
)

func tText(v string) item {
	return item{
		typ: itemText,
		val: v,
	}
}

func tId(v string) item {
	return item{
		typ: itemIdentifier,
		val: v,
	}
}

func tNum(v string) item {
	return item{
		typ: itemNumber,
		val: v,
	}
}

func tQuote(v string) item {
	return item{
		typ: itemString,
		val: v,
	}
}

func tRawQuote(v string) item {
	return item{
		typ: itemRawString,
		val: v,
	}
}

var lexTests = []lexTest{
	{"empty", "", []item{tEOF}},
	{"basic text", "hello", []item{tText("hello"), tEOF}},
	{"spaces", "  \t\n", []item{tText("  \t\n"), tEOF}},
	{"braces", "foo { bar { { } }} xxx", []item{
		tText("foo { bar { { } }} xxx"),
		tEOF}},
	{"action-id", "foo {{  x  }} bar", []item{
		tText("foo "),
		tLeft,
		tId("x"),
		tRight,
		tText(" bar"),
		tEOF}},
	{"multiple-actions", "y {{ a b }} {{ c }} d {{ e }}", []item{
		tText("y "),
		tLeft,
		tId("a"),
		tId("b"),
		tRight,
		tText(" "),
		tLeft,
		tId("c"),
		tRight,
		tText(" d "),
		tLeft,
		tId("e"),
		tRight,
		tEOF}},
	{"ids", "{{ tAG t8e __6 }}", []item{
		tLeft,
		tId("tAG"),
		tId("t8e"),
		tId("__6"),
		tRight,
		tEOF}},
	{"keywords", "{{if else range}}", []item{
		tLeft,
		tIf,
		tElse,
		tRange,
		tRight,
		tEOF}},
	{"pipes", "{{a | b | c}}", []item{
		tLeft,
		tId("a"),
		tPipe,
		tId("b"),
		tPipe,
		tId("c"),
		tRight,
		tEOF}},
	{"number", "{{x -2.3 0xfaFA +3 2.0e+8}}", []item{
		tLeft,
		tId("x"),
		tNum("-2.3"),
		tNum("0xfaFA"),
		tNum("+3"),
		tNum("2.0e+8"),
		tRight,
		tEOF}},
	{"quotes", `{{ "a str" x "another "}}`, []item{
		tLeft,
		tQuote(`"a str"`),
		tId("x"),
		tQuote(`"another "`),
		tRight,
		tEOF}},
	{"raw-quotes", "{{ `a str` x}}", []item{
		tLeft,
		tRawQuote("`a str`"),
		tId("x"),
		tRight,
		tEOF}},
}

func lexToSlice(input string) []item {
	var items []item
	l := Lex(input)
	for {
		item := l.NextItem()
		items = append(items, item)
		if item.typ == itemEOF || item.typ == itemError {
			break
		}
	}
	return items
}

func TestLexer(t *testing.T) {
	// The Rob Pike sample predates t.Run; I'm leaving it as it was originally
	// implemented.
	for _, test := range lexTests {
		items := lexToSlice(test.input)
		if !slicesEqual(items, test.items) {
			t.Errorf("%s: got\n\t%+v\nexpected\n\t%v", test.name, items, test.items)
		}
	}
}

type lexErrorTest struct {
	name      string
	input     string
	wantError string
}

var lexErrorTests = []lexErrorTest{
	{"unterminated-action", "{{", "unclosed"},
	{"unterminated-action-newline", "{{\n}}", "unclosed"},
	{"no-brace-in-action", "{{ { }}", "unrecognized"},
	{"no-comma-in-action", "{{ , }}", "unrecognized"},
	{"bad-number", "{{ 22k }}", "bad number"},
	{"unterminated-quote", "{{ \"22k }}", "unterminated quoted"},
	{"unterminated-raw-quote", "{{ `22k }}", "unterminated raw"},
}

func TestLexerErrors(t *testing.T) {
	for _, test := range lexErrorTests {
		items := lexToSlice(test.input)

		if len(items) == 0 {
			t.Errorf("%s: got no error token", test.name)
		} else {
			lastTok := items[len(items)-1]
			if lastTok.typ != itemError || !strings.Contains(lastTok.val, test.wantError) {
				t.Errorf("%s: got last token \"%+v\"\nexpected error containing %v", test.name, lastTok, test.wantError)
			}
		}
	}
}

// slicesEqual compares two slices and returns true iff they have equal elements
// compared with ==.
func slicesEqual[E comparable](s1, s2 []E) bool {
	if len(s1) != len(s2) {
		return false
	}
	for i := range s1 {
		if s1[i] != s2[i] {
			return false
		}
	}
	return true
}
