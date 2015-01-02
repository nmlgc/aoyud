/*
 * As of yet unnamed assembly-to-C decompiler.
 * Implemented in a similar fashion as the lexer from Go's own text/template
 * package. Operates in bytes to allow input files in any encoding.
 */

package main

import (
	"fmt"
	"gopkg.in/alecthomas/kingpin.v1"
	"io/ioutil"
	"log"
	"os"
)

func isLineBreak(b byte) bool {
	return (b == '\r') || (b == '\n')
}

// isWordDelim returns true if b is any whitespace character.
func isWhitespace(b byte) bool {
	return (b == ' ') || (b == '\t')
}

// isWordDelim returns true if b is any valid assembly word delimiter.
func isWordDelim(b byte) bool {
	return isWhitespace(b) || isLineBreak(b) ||
		(b == ';') || (b == ':') || (b == ',') || (b == '\'') || (b == '"')
}

// item represents a token or text string returned from the scanner.
type item struct {
	typ itemType // The type of this item.
	val []byte   // The value of this item.
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError itemType = iota // error occurred; value is text of error
	itemLabel                 // jump target
	itemRest                  // not yet lexed
)

type lexer struct {
	input       []byte
	stringDelim byte      // current string delimiter
	start       int       // start position of this item
	pos         int       // current position in the input
	items       chan item // channel of scanned items
}

type stateFn func(*lexer) stateFn

// lexString scans a string constant until it encounters the delimiter that
/// has been scanned before entering it (' or ").
func lexString(l *lexer) stateFn {
	for b := l.next(); ; b = l.next() {
		if isLineBreak(b) {
			return l.errorf("Missing end quote")
		} else if b == l.stringDelim {
			l.emit(itemRest)
			return lexInstruction
		}
	}
	return lexBase
}

// lexComment advances the input until the next line break.
// The ; has been scanned.
func lexComment(l *lexer) stateFn {
	for l.pos < len(l.input) {
		if b := l.next(); isLineBreak(b) {
			l.start = l.pos - 1
			return lexRest
		}
	}
	return nil
}

// lexLabel emits a label whose name ends at the current position.
// The : has been scanned.
func lexLabel(l *lexer) stateFn {
	l.pos--
	l.emit(itemLabel)
	l.pos++
	return lexBase
}

// lexRest emits the remaining unlexed code at a line break.
func lexRest(l *lexer) stateFn {
	l.emit(itemRest)
	return lexBase
}

// lexInstruction scans instructions and directives.
func lexInstruction(l *lexer) stateFn {
	for l.pos < len(l.input) {
		l.nextWord()
		switch b := l.next(); b {
		case '"', '\'':
			l.stringDelim = b
			return lexString
		default:
			return lexBreak(l, b)
		}
	}
	return nil
}

// lexBase branches both into labels and instructions.
func lexBase(l *lexer) stateFn {
	for l.pos < len(l.input) {
		l.nextWord()
		switch b := l.next(); b {
		case ':':
			return lexLabel
		default:
			return lexBreak(l, b)
		}
	}
	return nil
}

// lexBreak checks for cases that always end a line of code.
func lexBreak(l *lexer, b byte) stateFn {
	switch b {
	case ';':
		return lexComment
	case '\r', '\n':
		return lexRest
	default:
		l.emit(itemRest)
		return lexInstruction
	}
}

// next consumes the next byte in the input.
func (l *lexer) next() byte {
	l.pos++
	return l.input[l.pos-1]
}

// nextWord consumes the next whitespace-delimited word from the input.
func (l *lexer) nextWord() []byte {
	for isWhitespace(l.input[l.pos]) && l.pos < len(l.input)-1 {
		l.pos++
	}
	l.start = l.pos
	for !isWordDelim(l.input[l.pos]) && l.pos < len(l.input) {
		l.pos++
	}
	return l.input[l.start:l.pos]
}

// emit passes the current item back to the client.
func (l *lexer) emit(t itemType) {
	l.items <- item{t, l.input[l.start:l.pos]}
	l.start = l.pos
}

// errorf returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating the lexer.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{itemError, []byte(fmt.Sprintf(format, args...))}
	return nil
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for state := lexBase; state != nil; {
		state = state(l)
	}
	close(l.items)
}

// lex creates a new scanner for the input string.
func lex(input []byte) *lexer {
	l := &lexer{
		input: input,
		items: make(chan item),
	}
	go l.run()
	return l
}

func main() {
	filename := kingpin.Arg("filename", "Assembly file.").Required().ExistingFile()
	kingpin.Parse()

	bytes, err := ioutil.ReadFile(*filename)
	if err != nil {
		log.Fatalln(err)
	}
	l := lex(bytes)
	for i := range l.items {
		switch i.typ {
		case itemLabel:
			fmt.Printf("\n%s:", i.val)
		case itemError:
			fmt.Printf("\n**Error** %s\n", i.val)
		default:
			os.Stdout.Write(i.val)
		}
	}
}
