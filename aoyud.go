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

// instructions lists directives that can't be preceded by an identifier name.
var instructions = keywordGroup{
	"CALL", "INVOKE", "OPTION",
}

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
	typ    itemType // The type of this item
	val    []byte   // The main value of this item
	params [][]byte // Instruction parameters
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError       itemType = iota // error occurred; value is text of error
	itemLabel                       // jump target
	itemSymbol                      // symbol declaration
	itemInstruction                 // instruction or directive and its parameters
)

type lexer struct {
	input   []byte
	pos     int       // current position in the input
	curInst item      // current instruction being built
	items   chan item // channel of scanned items
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
	second := l.peekUntil(&wordDelim)
	// Instruction
	if !instructions.matches(first) && declarators.matches(second) {
		l.emitWord(itemSymbol, first)
		l.newInstruction(l.nextUntil(&wordDelim))
	} else if strings.EqualFold(string(first), "comment") {
		l.ignore(&whitespace)
		delim := charGroup{l.next()}
		l.nextUntil(&delim)
		l.nextUntil(&linebreak) // Yes, everything else on the line is ignored.
		return lexFirst
	} else {
		l.newInstruction(first)
	}
	return lexParam
}

// lexParam scans parameters and comments.
func lexParam(l *lexer) stateFn {
	if param := l.nextParam(); len(param) > 0 {
		l.curInst.params = append(l.curInst.params, param)
	}
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

// emitInstruction emits the currently cached instruction.
func (l *lexer) newInstruction(val []byte) {
	l.curInst.typ = itemInstruction
	if len(l.curInst.val) != 0 {
		l.items <- l.curInst
	}
	l.curInst.val = val
	l.curInst.params = nil
}

// emitWord emits the currently cached instruction, followed by the given word
// as the given item type.
func (l *lexer) emitWord(t itemType, word []byte) {
	l.newInstruction(nil)
	if len(word) > 0 {
		l.items <- item{t, word, nil}
	}
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for state := lexFirst; state != nil; {
		state = state(l)
	}
	l.newInstruction(nil) // send the currently cached instruction
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

func (i item) String() string {
	var format string
	switch i.typ {
	case itemLabel:
		format = "%s:\n"
	case itemSymbol:
		format = "%s"
	case itemInstruction:
		format = "\t%s"
	}
	ret := fmt.Sprintf(format, i.val)
	for num, param := range i.params {
		if num == 0 {
			ret += "\t"
		} else {
			ret += ", "
		}
		ret += fmt.Sprintf("%s", param)
	}
	if i.typ == itemInstruction {
		ret += "\n"
	}
	return ret
}

type parser struct {
	instructions []item
	// General state
	symLast string // last symbol declaration encountered
	// Open procedures
	procStart int
	procNest  int
	procName  string
}

// parseFn represents a function handling a certain instruction or directive.
type parseFn func(p *parser, itemNum int, i *item)

func (p *parser) parsePROC(itemNum int, i *item) {
	if p.procNest == 0 {
		p.procName = p.symLast
		p.procStart = itemNum
	} else {
		log.Printf("ignoring nested procedure %s\n", p.symLast)
	}
	p.procNest++
}

func (p *parser) parseENDP(itemNum int, i *item) {
	if p.procNest == 0 {
		log.Printf("ignoring procedure %s without a PROC directive\n", p.symLast)
	} else if p.procNest == 1 {
		log.Printf(
			"found procedure %s ranging from lex items #%d-#%d\n",
			p.procName, p.procStart, itemNum,
		)
	}
	p.procNest--
}

var parseFns = map[string]parseFn{
	"PROC": (*parser).parsePROC,
	"ENDP": (*parser).parseENDP,
}

func (p *parser) eval(i *item) {
	itemNum := len(p.instructions)
	valString := string(i.val)

	p.instructions = append(p.instructions, *i)
	switch i.typ {
	case itemSymbol:
		p.symLast = valString
	case itemInstruction:
		valUpper := strings.ToUpper(valString)
		if insFunc := parseFns[valUpper]; insFunc != nil {
			insFunc(p, itemNum, i)
		}
	}
}

func (p *parser) end() {
	if p.procNest != 0 {
		log.Printf("ignoring procedure %s without an ENDP directive\n", p.procName)
	}
}

func main() {
	filename := kingpin.Arg("filename", "Assembly file.").Required().ExistingFile()
	kingpin.Parse()

	bytes, err := ioutil.ReadFile(*filename)
	if err != nil {
		log.Fatalln(err)
	}
	log.SetFlags(0)
	log.SetPrefix(*filename + ": ")
	l := lex(bytes)
	var p parser

	for i := range l.items {
		p.eval(&i)
		fmt.Print(i)
	}
	p.end()
}
