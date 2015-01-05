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
	"strings"
)

type charGroup []byte
type keywordGroup []string

// declarators lists directives that are preceded by an identifier name.
var declarators = keywordGroup{
	"DB", "DW", "DD", "DQ", "DT", "DP", "DF", // data
	"=", "EQU", "TEXTEQU", "LABEL", // labels
	"MACRO", "TYPEDEF", // macros
	"CATSTR", "SUBSTR", "INSTR", "SIZESTR", // string macros
	"PROC", "ENDP", // procedures
	"STRUC", "STRUCT", "ENDS", // structures
	"SEGMENT", "ENDS", // segments
	"GROUP", // groups
}

const eof = 0

var linebreak = charGroup{'\r', '\n'}
var whitespace = charGroup{' ', '\t'}
var paramDelim = append(charGroup{',', ';'}, linebreak...)
var wordDelim = append(append(charGroup{':'}, whitespace...), paramDelim...)

// nestLevelEnter and nestLevelLeave map the various punctuation marks used in
// TASM's syntax to bit flags ordered by their respective nesting priorities.
var nestLevelEnter = map[byte]int{
	'{':  1,
	'(':  2,
	'<':  4,
	'"':  8,
	'\'': 8,
}
var nestLevelLeave = map[byte]int{
	'}':  1,
	')':  2,
	'>':  4,
	'"':  8,
	'\'': 8,
}

func (g *charGroup) matches(b byte) bool {
	for _, v := range *g {
		if v == b {
			return true
		}
	}
	return false
}

func (g *keywordGroup) matches(word []byte) bool {
	if len(word) == 0 {
		return false
	}
	wordString := string(word)
	for _, v := range *g {
		if strings.EqualFold(wordString, v) {
			return true
		}
	}
	return false
}

// item represents a token or text string returned from the scanner.
type item struct {
	typ itemType // The type of this item.
	val []byte   // The value of this item.
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError       itemType = iota // error occurred; value is text of error
	itemLabel                       // jump target
	itemSymbol                      // symbol declaration
	itemInstruction                 // name of an instruction or directive
	itemParam                       // generic parameter
)

type lexer struct {
	input []byte
	pos   int       // current position in the input
	items chan item // channel of scanned items
}

type stateFn func(*lexer) stateFn

// lexFirst scans labels, the symbol declaration, and the name of the
// instruction.
func lexFirst(l *lexer) stateFn {
	first := l.nextUntil(&wordDelim)
	// Label?
	if l.peek() == ':' {
		l.next()
		l.emitWord(itemLabel, first)
		return lexFirst
	}
	// Instruction
	if declarators.matches(l.peekUntil(&wordDelim)) {
		l.emitWord(itemSymbol, first)
		l.emitWord(itemInstruction, l.nextUntil(&wordDelim))
	} else {
		l.emitWord(itemInstruction, first)
	}
	return lexParam
}

// lexParam scans parameters and comments.
func lexParam(l *lexer) stateFn {
	l.emitWord(itemParam, l.nextParam())
	switch l.next() {
	case ';', '\\':
		// Comment
		l.nextUntil(&linebreak)
		return lexFirst
	case '\r', '\n':
		return lexFirst
	case eof:
		return nil
	}
	return lexParam
}

// ignore consumes bytes from the input until they stop matching the given
// character group.
func (l *lexer) ignore(delim *charGroup) {
	for delim.matches(l.peek()) {
		l.next()
	}
}

// peek returns but does not consume the next byte in the input.
func (l *lexer) peek() byte {
	if l.pos >= len(l.input) {
		return eof
	}
	return l.input[l.pos]
}

// next consumes the next byte in the input.
func (l *lexer) next() byte {
	ret := l.peek()
	l.pos++
	return ret
}

// peekUntil returns but does not consume the next word that is delimited by
// the given character group.
func (l *lexer) peekUntil(delim *charGroup) []byte {
	pos := l.pos
	ret := l.nextUntil(delim)
	l.pos = pos
	return ret
}

// nextUntil consumes the next word that is delimited by the given character group.
func (l *lexer) nextUntil(delim *charGroup) []byte {
	l.ignore(&whitespace)
	start := l.pos
	for !delim.matches(l.peek()) && l.peek() != eof {
		l.next()
	}
	return l.input[start:l.pos]
}

// nextParam consumes and returns the next parameter to an instruction, taking
// nesting into account.
func (l *lexer) nextParam() []byte {
	var quote byte
	level := 0

	l.ignore(&whitespace)
	start := l.pos
	for !(level == 0 && paramDelim.matches(l.peek())) && l.peek() != eof {
		b := l.next()

		if level == 0 && b == '\\' {
			l.nextUntil(&linebreak)
			l.ignore(&linebreak)
		}
		var leavecond bool
		ll := nestLevelLeave[b]
		if quote != 0 {
			leavecond = (b == quote)
		} else {
			leavecond = (level & ll) != 0
		}
		if leavecond {
			level &= ^ll
			quote = 0
		} else if le := nestLevelEnter[b]; le > level {
			level |= le
			if b == '\'' || b == '"' {
				quote = b
			}
		}
	}
	for l.pos > start && whitespace.matches(l.input[l.pos-1]) {
		l.pos--
	}
	return l.input[start:l.pos]
}

// emitWord emits the given word as the given item type.
func (l *lexer) emitWord(t itemType, word []byte) {
	if len(word) > 0 {
		l.items <- item{t, word}
	}
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for state := lexFirst; state != nil; {
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

	lastType := itemError
	for i := range l.items {
		switch i.typ {
		case itemLabel:
			fmt.Printf("\n%s:\n", i.val)
		case itemSymbol:
			fmt.Printf("\n%s", i.val)
		case itemInstruction:
			if lastType == itemParam || lastType == itemInstruction {
				fmt.Println("")
			}
			fmt.Printf("\t%s", i.val)
		case itemParam:
			if lastType == itemParam {
				fmt.Printf(", ")
			} else if lastType == itemInstruction {
				fmt.Printf("\t")
			}
			fmt.Printf("%s", i.val)
		}
		lastType = i.typ
	}
}
