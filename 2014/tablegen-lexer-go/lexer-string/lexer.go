// Lexer for the TableGen language (https://llvm.org/docs/TableGen/)
//
// Works the same way as the lexer in the parent directory, but acts on a
// string rather than []byte; this lets the lexer return subslices of the
// string as token values, without allocations.
//
// This lexer's fidelity could be improved (it supports unicode in strings, not
// in identifiers, and it lacks handling of base-prefix numbers like 0b111), but
// its main goal is for performance comparisons.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"unicode/utf8"
)

// TokenName is a type for describing tokens mnemonically.
type TokenName int

// Token represents a single token in the input stream.
// Name: mnemonic name (numeric).
// Val: string value of the token from the original stream.
// Pos: position - offset from beginning of stream.
type Token struct {
	Name TokenName
	Val  string
	Pos  int
}

// Values for TokenName
const (
	// Special tokens
	ERROR TokenName = iota
	EOF

	COMMENT
	IDENTIFIER
	NUMBER
	QUOTE

	// Operators
	PLUS
	MINUS
	MULTIPLY
	DIVIDE
	PERIOD
	BACKSLASH
	COLON
	PERCENT
	PIPE
	EXCLAMATION
	QUESTION
	POUND
	AMPERSAND
	SEMI
	COMMA
	L_PAREN
	R_PAREN
	L_ANG
	R_ANG
	L_BRACE
	R_BRACE
	L_BRACKET
	R_BRACKET
	EQUALS
)

var tokenNames = [...]string{
	ERROR:       "ERROR",
	EOF:         "EOF",
	COMMENT:     "COMMENT",
	IDENTIFIER:  "IDENTIFIER",
	NUMBER:      "NUMBER",
	QUOTE:       "QUOTE",
	PLUS:        "PLUS",
	MINUS:       "MINUS",
	MULTIPLY:    "MULTIPLY",
	DIVIDE:      "DIVIDE",
	PERIOD:      "PERIOD",
	BACKSLASH:   "BACKSLASH",
	COLON:       "COLON",
	PERCENT:     "PERCENT",
	PIPE:        "PIPE",
	EXCLAMATION: "EXCLAMATION",
	QUESTION:    "QUESTION",
	POUND:       "POUND",
	AMPERSAND:   "AMPERSAND",
	SEMI:        "SEMI",
	COMMA:       "COMMA",
	L_PAREN:     "L_PAREN",
	R_PAREN:     "R_PAREN",
	L_ANG:       "L_ANG",
	R_ANG:       "R_ANG",
	L_BRACE:     "L_BRACE",
	R_BRACE:     "R_BRACE",
	L_BRACKET:   "L_BRACKET",
	R_BRACKET:   "R_BRACKET",
	EQUALS:      "EQUALS",
}

func (tok Token) String() string {
	return fmt.Sprintf("Token{%s, '%s', %d}", tokenNames[tok.Name], tok.Val, tok.Pos)
}

func makeErrorToken(pos int) Token {
	return Token{ERROR, "", pos}
}

// Operator table for lookups.
var opTable = [...]TokenName{
	'+':  PLUS,
	'-':  MINUS,
	'*':  MULTIPLY,
	'/':  DIVIDE,
	'.':  PERIOD,
	'\\': BACKSLASH,
	':':  COLON,
	'%':  PERCENT,
	'|':  PIPE,
	'!':  EXCLAMATION,
	'?':  QUESTION,
	'#':  POUND,
	'&':  AMPERSAND,
	';':  SEMI,
	',':  COMMA,
	'(':  L_PAREN,
	')':  R_PAREN,
	'<':  L_ANG,
	'>':  R_ANG,
	'{':  L_BRACE,
	'}':  R_BRACE,
	'[':  L_BRACKET,
	']':  R_BRACKET,
	'=':  EQUALS,
}

// Lexer
//
// Create a new lexer with NewLexer and then call NextToken repeatedly to get
// tokens from the stream. The lexer will return a token with the name EOF when
// done.
type Lexer struct {
	buf string

	// Current rune.
	r rune

	// Position of the current rune in buf.
	rpos int

	// Position of the next rune in buf.
	nextpos int
}

// NewLexer creates a new lexer for the given input.
func NewLexer(buf string) *Lexer {
	lex := Lexer{buf, -1, 0, 0}

	// Prime the lexer by calling .next
	lex.next()
	return &lex
}

func (lex *Lexer) NextToken() Token {
	// Skip non-tokens like whitespace and check for EOF.
	lex.skipNontokens()
	if lex.r < 0 {
		return Token{EOF, "", lex.nextpos}
	}

	// Is this an operator?
	if int(lex.r) < len(opTable) {
		if opName := opTable[lex.r]; opName != ERROR {
			if opName == DIVIDE {
				// Special case: '/' may be the start of a comment.
				if lex.peekNextByte() == '/' {
					return lex.scanComment()
				}
			}
			startpos := lex.rpos
			lex.next()
			return Token{opName, string(lex.buf[startpos:lex.rpos]), startpos}
		}
	}

	// Not an operator. Try other types of tokens.
	if isAlpha(lex.r) {
		return lex.scanIdentifier()
	} else if isDigit(lex.r) {
		return lex.scanNumber()
	} else if lex.r == '"' {
		return lex.scanQuote()
	}

	return makeErrorToken(lex.rpos)
}

// next advances the lexer's internal state to point to the next rune in the
// input.
func (lex *Lexer) next() {
	if lex.nextpos < len(lex.buf) {
		lex.rpos = lex.nextpos
		r, w := rune(lex.buf[lex.nextpos]), 1

		if r >= utf8.RuneSelf {
			r, w = utf8.DecodeRuneInString(lex.buf[lex.nextpos:])
		}

		lex.nextpos += w
		lex.r = r
	} else {
		lex.rpos = len(lex.buf)
		lex.r = -1 // EOF
	}
}

// peekNextByte returns the next byte in the stream (the one after lex.r).
// Note: a single byte is peeked at - if there's a rune longer than a byte
// there, only its first byte is returned.
func (lex *Lexer) peekNextByte() rune {
	if lex.nextpos < len(lex.buf) {
		return rune(lex.buf[lex.nextpos])
	} else {
		return -1
	}
}

func (lex *Lexer) skipNontokens() {
	for lex.r == ' ' || lex.r == '\t' || lex.r == '\n' || lex.r == '\r' {
		lex.next()
	}
}

func (lex *Lexer) scanIdentifier() Token {
	startpos := lex.rpos
	for isAlpha(lex.r) || isDigit(lex.r) {
		lex.next()
	}
	return Token{IDENTIFIER, lex.buf[startpos:lex.rpos], startpos}
}

func (lex *Lexer) scanNumber() Token {
	startpos := lex.rpos
	for isDigit(lex.r) {
		lex.next()
	}
	return Token{NUMBER, lex.buf[startpos:lex.rpos], startpos}
}

func (lex *Lexer) scanQuote() Token {
	startpos := lex.rpos
	lex.next()
	for lex.r > 0 && lex.r != '"' {
		lex.next()
	}

	if lex.r < 0 {
		return makeErrorToken(startpos)
	} else {
		lex.next()
		return Token{QUOTE, string(lex.buf[startpos:lex.rpos]), startpos}
	}
}

func (lex *Lexer) scanComment() Token {
	startpos := lex.rpos
	lex.next()
	for lex.r > 0 && lex.r != '\n' {
		lex.next()
	}

	tok := Token{COMMENT, string(lex.buf[startpos:lex.rpos]), startpos}
	lex.next()
	return tok
}

func isAlpha(r rune) bool {
	return 'a' <= r && r <= 'z' || 'A' <= r && r <= 'Z' || r == '_' || r == '$'
}

func isDigit(r rune) bool {
	return '0' <= r && r <= '9'
}

func tokenizeAllAppend(buf string) []Token {
	var toks []Token

	lex := NewLexer(buf)
	for {
		tok := lex.NextToken()
		toks = append(toks, tok)
		if tok.Name == EOF {
			break
		}
	}

	return toks
}

func tokenizeAllPrealloc(buf string) []Token {
	toks := make([]Token, 0, 200000)

	lex := NewLexer(buf)
	for {
		tok := lex.NextToken()
		toks = append(toks, tok)
		if tok.Name == EOF {
			break
		}
	}

	return toks
}
