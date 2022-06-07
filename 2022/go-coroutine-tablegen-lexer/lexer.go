// Coroutine-style lexer for TableGen.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"unicode"
	"unicode/utf8"
)

// Token represents a single token in the input stream.
// Name: mnemonic name (numeric).
// Val: string value of the token from the original stream.
// Pos: position - offset from beginning of stream.
type Token struct {
	Name TokenName
	Val  string
	Pos  int
}

type TokenName int

const (
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

const eof = -1

type Lexer struct {
	input  string     // the string being scanned
	start  int        // start position of this tokens
	pos    int        // current position of the input
	width  int        // width of last rune read from input
	tokens chan Token // channel of scanned tokens
}

type stateFn func(*Lexer) stateFn

// Lex creates a new Lexer
func Lex(input string) *Lexer {
	l := &Lexer{
		input:  input,
		tokens: make(chan Token),
	}
	go l.run()
	return l
}

// NextToken returns the next item from the input. The Lexer has to be
// drained (all tokens received until itemEOF or itemError) - otherwise
// the Lexer goroutine will leak.
func (l *Lexer) NextToken() Token {
	return <-l.tokens
}

// run runs the lexer - should be run in a separate goroutine.
func (l *Lexer) run() {
	for state := lexText; state != nil; {
		state = state(l)
	}
	close(l.tokens) // no more tokens will be delivered
}

func (l *Lexer) emit(name TokenName) {
	l.tokens <- Token{
		Name: name,
		Val:  l.input[l.start:l.pos],
		Pos:  l.start,
	}
	l.start = l.pos
}

// next advances to the next rune in input and returns it
func (l *Lexer) next() (r rune) {
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += l.width
	return r
}

// ignore skips over the pending input before this point
func (l *Lexer) ignore() {
	l.start = l.pos
}

// backup steps back one rune. Can be called only once per call of next.
func (l *Lexer) backup() {
	l.pos -= l.width
}

// peek returns but does not consume the next run in the input.
func (l *Lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// errorf returns an error token and terminates the scan by passing back
// a nil pointer that will be the next state.
func (l *Lexer) errorf(format string, args ...interface{}) stateFn {
	l.tokens <- Token{
		Name: ERROR,
		Val:  fmt.Sprintf(format, args...),
		Pos:  l.pos,
	}
	return nil
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

func lexText(l *Lexer) stateFn {
	for {
		switch r := l.next(); {
		case r == eof:
			l.emit(EOF)
			return nil
		case r == ' ' || r == '\t' || r == '\n' || r == '\r':
			l.ignore()
		case int(r) < len(opTable) && opTable[r] != ERROR:
			op := opTable[r]
			if op == DIVIDE && l.peek() == '/' {
				return lexComment
			}
			l.emit(op)
		case isAlpha(r):
			l.backup()
			return lexIdentifier
		case isDigit(r):
			l.backup()
			return lexNumber
		case r == '"':
			return lexQuote
		}
	}
}

func lexIdentifier(l *Lexer) stateFn {
	for isAlphaNumeric(l.next()) {
	}

	// Now the current rune is no longer alphanumeric, so back up to the last
	// alphanumeric rune and emit the current token.
	l.backup()
	l.emit(IDENTIFIER)
	return lexText
}

func lexNumber(l *Lexer) stateFn {
	for isDigit(l.next()) {
	}
	l.backup()
	l.emit(NUMBER)
	return lexText
}

func lexQuote(l *Lexer) stateFn {
	for {
		switch l.next() {
		case eof:
			return l.errorf("unterminated quoted string")
		case '"':
			l.emit(QUOTE)
			return lexText
		}
	}
}

func lexComment(l *Lexer) stateFn {
	r := l.next()
	for r != eof && r != '\n' {
		r = l.next()
	}
	l.backup()
	l.emit(COMMENT)
	return lexText
}

// isAlpha reports whether r is an alphabetic or underscore.
func isAlpha(r rune) bool {
	return r == '_' || unicode.IsLetter(r)
}

// isAlphaNumeric reports whether r is an alphabetic, digit, or underscore.
func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// isDigit reports whether r is a digit.
func isDigit(r rune) bool {
	return unicode.IsDigit(r)
}

func tokenizeAllAppend(input string) []Token {
	var tokens []Token
	l := Lex(input)
	for {
		token := l.NextToken()
		tokens = append(tokens, token)
		if token.Name == EOF || token.Name == ERROR {
			break
		}
	}
	return tokens
}

func tokenizeAllPrealloc(input string) []Token {
	tokens := make([]Token, 0, 200000)
	l := Lex(input)
	for {
		token := l.NextToken()
		tokens = append(tokens, token)
		if token.Name == EOF || token.Name == ERROR {
			break
		}
	}
	return tokens
}
