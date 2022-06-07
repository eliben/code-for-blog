// Lexer based on Rob Pike's talk https://talks.golang.org/2011/lex.slide#1
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

type item struct {
	typ itemType
	val string
}

func (i item) String() string {
	switch i.typ {
	case itemEOF:
		return "EOF"
	case itemError:
		return i.val
	}
	if len(i.val) > 10 {
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

type itemType int

// itemType identifies the type of lex items
const (
	itemError itemType = iota // error occurred; value is text of error
	itemEOF
	itemIdentifier
	itemLeftMeta
	itemNumber
	itemPipe
	itemRawString
	itemRightMeta
	itemString
	itemText

	// keywors appear after this one: this item is only used to delimit them.
	itemKeyword

	itemIf
	itemElse
	itemEnd
	itemRange
)

// keywords maps string keywords to item* constants; the default value returned
// is 0, which is less than the itemKeyword delimiter.
var keywords = map[string]itemType{
	"else":  itemElse,
	"end":   itemEnd,
	"if":    itemIf,
	"range": itemRange,
}

const eof = -1
const leftMeta = "{{"
const rightMeta = "}}"

type Lexer struct {
	input string    // the string being scanned
	start int       // start position of this item
	pos   int       // current position of the input
	width int       // width of last rune read from input
	items chan item // channel of scanned items
}

// stateFn represents the state of the scanner as a function
// that returns the next state.
type stateFn func(*Lexer) stateFn

// Lex creates a new Lexer
func Lex(input string) *Lexer {
	l := &Lexer{
		input: input,
		items: make(chan item),
	}
	go l.run()
	return l
}

// NextItem returns the next item from the input. The Lexer has to be
// drained (all items received until itemEOF or itemError) - otherwise
// the Lexer goroutine will leak.
func (l *Lexer) NextItem() item {
	return <-l.items
}

// run runs the lexer - should be run in a separate goroutine.
func (l *Lexer) run() {
	for state := lexText; state != nil; {
		state = state(l)
	}
	close(l.items) // no more tokens will be delivered
}

func (l *Lexer) emit(t itemType) {
	l.items <- item{t, l.input[l.start:l.pos]}
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
	l.items <- item{
		itemError,
		fmt.Sprintf(format, args...),
	}
	return nil
}

// accept consumes the next rune if it's from the valid set.
func (l *Lexer) accept(valid string) bool {
	if strings.IndexRune(valid, l.next()) >= 0 {
		return true
	}
	l.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *Lexer) acceptRun(valid string) {
	for strings.IndexRune(valid, l.next()) >= 0 {
	}
	l.backup()
}

func lexText(l *Lexer) stateFn {
	for {
		if strings.HasPrefix(l.input[l.pos:], leftMeta) {
			if l.pos > l.start {
				l.emit(itemText)
			}
			return lexLeftMeta
		}
		if l.next() == eof {
			break
		}
	}

	// EOF was reached while still in the text state.
	if l.pos > l.start {
		l.emit(itemText)
	}
	l.emit(itemEOF)
	return nil
}

func lexLeftMeta(l *Lexer) stateFn {
	l.pos += len(leftMeta)
	l.emit(itemLeftMeta)
	return lexInsideAction // now inside {{ }}
}

func lexRightMeta(l *Lexer) stateFn {
	l.pos += len(rightMeta)
	l.emit(itemRightMeta)
	return lexText
}

// isSpace reports whether r is a space character.
func isSpace(r rune) bool {
	return r == ' ' || r == '\t' || r == '\r' || r == '\n'
}

// isAlphaNumeric reports whether r is an alphabetic, digit, or underscore.
func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

func lexInsideAction(l *Lexer) stateFn {
	// Either number, quoted string or identifier.
	// Spaces spearate and are ignored.
	// Pipe symbols separate and are emitted.
	for {
		if strings.HasPrefix(l.input[l.pos:], rightMeta) {
			return lexRightMeta
		}
		switch r := l.next(); {
		case r == eof || r == '\n':
			return l.errorf("unclosed action")
		case isSpace(r):
			l.ignore()
		case r == '|':
			l.emit(itemPipe)
		case r == '"':
			return lexQuote
		case r == '`':
			return lexRawQuote
		case r == '+' || r == '-' || '0' <= r && r <= '9':
			l.backup()
			return lexNumber
		case isAlphaNumeric(r):
			l.backup()
			return lexIdentifier
		default:
			return l.errorf("unrecognized character in action: %#U", r)
		}
	}
}

func lexNumber(l *Lexer) stateFn {
	// Optional leading sign.
	l.accept("+-")
	// is it hex?
	digits := "0123456789"
	if l.accept("0") && l.accept("xX") {
		digits = "0123456789abcdefABCDEF"
	}
	l.acceptRun(digits)
	if l.accept(".") {
		l.acceptRun(digits)
	}
	if l.accept("eE") {
		l.accept("+-")
		l.acceptRun("0123456789")
	}
	if isAlphaNumeric(l.peek()) {
		l.next()
		return l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
	}
	l.emit(itemNumber)
	return lexInsideAction
}

func lexQuote(l *Lexer) stateFn {
	for {
		switch l.next() {
		case '\\':
			if r := l.next(); r != eof && r != '\n' {
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("unterminated quoted string")
		case '"':
			l.emit(itemString)
			return lexInsideAction
		}
	}
}

// lexRawQuote scans a raw quoted string.
func lexRawQuote(l *Lexer) stateFn {
	for {
		switch l.next() {
		case eof:
			return l.errorf("unterminated raw quoted string")
		case '`':
			l.emit(itemRawString)
			return lexInsideAction
		}
	}
}

// lexIdentifier scans an alphanumeric.
func lexIdentifier(l *Lexer) stateFn {
	// Advance as long as the current rune is alphanumeric
	for isAlphaNumeric(l.next()) {
	}

	// Now the current rune is no longer alphanumeric, so back up to the last
	// alphanumeric rune and emit the current item.
	l.backup()
	if item := keywords[l.input[l.start:l.pos]]; item > itemKeyword {
		l.emit(item)
	} else {
		l.emit(itemIdentifier)
	}
	return lexInsideAction
}

func main() {
	l := Lex("hi {{ id }}")
	for {
		item := l.NextItem()
		fmt.Println(item)
		if item.typ == itemEOF || item.typ == itemError {
			break
		}
	}
}
