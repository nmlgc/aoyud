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
	itemSymbol                      // Symbol declaration
	itemInstruction                 // name of an instruction or directive
	itemParam                       // generic parameter
)

type lexer struct {
	input     []byte
	firstWord []byte    // first word on a line
	start     int       // start position of this item
	pos       int       // current position in the input
	items     chan item // channel of scanned items
}

type stateFn func(*lexer) stateFn

// lexComment advances the input until the next line break.
// The ; has been scanned.
func lexComment(l *lexer) stateFn {
	l.nextUntil(&linebreak)
	l.pos++
	return lexBase
}

// lexInstruction scans instructions, directives and their parameters.
func lexInstruction(l *lexer) stateFn {
	if l.firstWord != nil {
		if declarators.matches(l.peekUntil(&wordDelim)) {
			l.emitWord(itemSymbol, l.firstWord)
			l.emitWord(itemInstruction, l.nextUntil(&wordDelim))
		} else {
			l.emitWord(itemInstruction, l.firstWord)
		}
		l.firstWord = nil
	}
	l.emitWord(itemParam, l.nextParam())
	if l.pos < len(l.input) {
		return lexBreak(l, l.next())
	}
	return nil
}

// lexBase branches both into labels and instructions.
func lexBase(l *lexer) stateFn {
	l.firstWord = l.nextUntil(&wordDelim)
	switch b := l.next(); b {
	case ':':
		l.emitWord(itemLabel, l.firstWord)
	case '\r', '\n':
		// Parameterless instructions
		if len(l.firstWord) > 0 {
			l.emitWord(itemInstruction, l.firstWord)
		}
	default:
		return lexBreak(l, b)
	}
	return lexBase
}

// lexBreak checks for cases that always end a line of code.
func lexBreak(l *lexer, b byte) stateFn {
	if b == ';' || b == '\\' {
		return lexComment
	} else if linebreak.matches(b) {
		return lexBase
	}
	return lexInstruction
}

// next consumes the next byte in the input.
func (l *lexer) next() byte {
	l.pos++
	return l.input[l.pos-1]
}

// ignore consumes bytes from the input until they stop matching the given
// character group.
func (l *lexer) ignore(delim *charGroup) {
	for delim.matches(l.input[l.pos]) && l.pos < len(l.input) {
		l.pos++
	}
}

// nextUntil consumes the next word that is delimited by the given character group.
func (l *lexer) nextUntil(delim *charGroup) []byte {
	l.ignore(&whitespace)
	l.start = l.pos
	for !delim.matches(l.input[l.pos]) && l.pos < len(l.input) {
		l.pos++
	}
	return l.input[l.start:l.pos]
}

// nextParam consumes and returns the next parameter to an instruction, taking
// nesting into account.
func (l *lexer) nextParam() []byte {
	var quote byte
	level := 0

	l.ignore(&whitespace)
	l.start = l.pos
	for !(level == 0 && paramDelim.matches(l.input[l.pos])) && l.pos < len(l.input) {
		b := l.next()

		if level == 0 && b == '\\' {
			// TODO: Find a nicer way to work around \r\n.
			l.ignore(&whitespace)
			if l.next() == '\r' && l.input[l.pos] == '\n' {
				l.next()
			}
			continue
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
	for l.pos > l.start && whitespace.matches(l.input[l.pos-1]) {
		l.pos--
	}
	return l.input[l.start:l.pos]
}

func (l *lexer) peekUntil(delim *charGroup) []byte {
	pos := l.pos
	ret := l.nextUntil(delim)
	l.pos = pos
	return ret
}

// emitWord emits the given word as the given item type.
func (l *lexer) emitWord(t itemType, word []byte) {
	if len(word) > 0 {
		l.items <- item{t, word}
	}
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for state := lexBase; state != nil && l.pos < len(l.input); {
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
