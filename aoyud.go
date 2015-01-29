/*
 * As of yet unnamed assembly-to-C decompiler.
 * Implemented in a similar fashion as the lexer from Go's own text/template
 * package.
 */

package main

import (
	"fmt"
	"gopkg.in/alecthomas/kingpin.v1"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
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

// conditionals lists all conditional directives that aren't kept in the parse
// list.
var conditionals = keywordGroup{
	"IFDEF", "IFNDEF", "IF", "IFE", "IFB", "IFNB",
	"IFIDN", "IFIDNI", "IFDIF", "IFDIFI",
	"ELSEIFDEF", "ELSEIFNDEF", "ELSEIF", "ELSEIFE", "ELSEIFB", "ELSEIFNB",
	"ELSEIFIDN", "ELSEIFIDNI", "ELSEIFDIF", "ELSEIFDIFI",
	"ELSE", "ENDIF",
}

var macros = keywordGroup{
	"MACRO", "FOR", "FORC", "REPT", "REPEAT", "WHILE", "IRP", "IRPC", "ENDM",
}

var linebreak = charGroup{'\r', '\n'}
var whitespace = charGroup{' ', '\t'}
var paramDelim = append(charGroup{',', ';'}, linebreak...)
var wordDelim = append(append(charGroup{':'}, whitespace...), paramDelim...)
var insDelim = append(charGroup{'='}, wordDelim...)
var shuntDelim = append(charGroup{
	'+', '-', '*', '/', '|', '(', ')', '[', ']', '<', '>', ':', '&', '"',
}, whitespace...)

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

func (g *keywordGroup) matches(word string) bool {
	if len(word) == 0 {
		return false
	}
	for _, v := range *g {
		if strings.EqualFold(word, v) {
			return true
		}
	}
	return false
}

func (g *keywordGroup) matchesInstruction(i *item) bool {
	return i.typ == itemInstruction && g.matches(i.val)
}

// item represents a token or text string returned from the scanner.
type item struct {
	typ    itemType // The type of this item
	sym    string   // Optional symbol name
	val    string   // Name of the instruction or label. Limited to ASCII characters.
	params []string // Instruction parameters
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError       itemType = iota // error occurred; value is text of error
	itemLabel                       // jump target
	itemInstruction                 // instruction or directive and its parameters
)

type lexer struct {
	stream  lexStream
	paths   []string  // search paths for relative includes
	curInst item      // current instruction being built
	items   chan item // channel of scanned items
}

type stateFn func(*lexer) stateFn

// lexFn represents a function handling a certain instruction or directive
// at lexing time.
type lexFn struct {
	f         func(l *lexer, i *item)
	minParams int
}

func (l *lexer) lexINCLUDE(i *item) {
	// Remember to restore the old filename once we're done with this one
	defer log.SetPrefix(log.Prefix())
	newL := lexFile(i.params[0], l.paths)
	for i := range newL.items {
		l.items <- i
	}
}

// lexFirst scans labels, the symbol declaration, and the name of the
// instruction.
func lexFirst(l *lexer) stateFn {
	first := l.stream.nextUntil(&insDelim)
	// Label?
	if l.stream.peek() == ':' {
		l.stream.next()
		l.emitItem(item{typ: itemLabel, sym: first})
		return lexFirst
	}
	// Assignment? (Needs to be a special case because = doesn't need to be
	// surrounded by spaces, and nextUntil() isn't designed to handle that.)
	if l.stream.peek() == '=' {
		l.newInstruction(first, string(l.stream.next()))
	} else if second := l.stream.peekUntil(&wordDelim); !instructions.matches(first) && declarators.matches(second) {
		l.newInstruction(first, l.stream.nextUntil(&wordDelim))
	} else if strings.EqualFold(first, "comment") {
		l.stream.ignore(&whitespace)
		delim := charGroup{l.stream.next()}
		l.stream.nextUntil(&delim)
		l.stream.nextUntil(&linebreak) // Yes, everything else on the line is ignored.
		return lexFirst
	} else {
		l.newInstruction("", first)
	}
	return lexParam
}

// lexParam scans parameters and comments.
func lexParam(l *lexer) stateFn {
	if param := l.stream.nextParam(); len(param) > 0 {
		l.curInst.params = append(l.curInst.params, param)
	}
	switch l.stream.next() {
	case ';', '\\':
		// Comment
		l.stream.nextUntil(&linebreak)
		return lexFirst
	case '\r', '\n':
		return lexFirst
	case eof:
		return nil
	}
	return lexParam
}

// emitInstruction emits the currently cached instruction.
func (l *lexer) newInstruction(sym, val string) {
	// Nope, turning this global would result in an initialization loop.
	var lexFns = map[string]lexFn{
		"INCLUDE": {(*lexer).lexINCLUDE, 1},
	}

	l.curInst.typ = itemInstruction
	lexFunc, ok := lexFns[strings.ToUpper(l.curInst.val)]
	if ok && l.curInst.checkMinParams(lexFunc.minParams) {
		lexFunc.f(l, &l.curInst)
	} else if len(l.curInst.val) > 0 {
		l.items <- l.curInst
	}
	l.curInst.sym = sym
	l.curInst.val = val
	l.curInst.params = nil
}

// emitItem emits the currently cached instruction, followed by the given item.
func (l *lexer) emitItem(i item) {
	l.newInstruction("", "")
	if len(i.val) > 0 || len(i.sym) > 0 {
		l.items <- i
	}
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for state := lexFirst; state != nil; {
		state = state(l)
	}
	l.newInstruction("", "") // send the currently cached instruction
	close(l.items)
}

// readFirstFromPaths reads and returns the contents of a file with name
// filename from the first directory in the given list that contains such a
// file, as well as the full path to the file that was read.
func readFirstFromPaths(filename string, paths []string) (string, string) {
	for _, path := range paths {
		fullname := filepath.Join(path, filename)
		bytes, err := ioutil.ReadFile(fullname)
		if err == nil {
			return string(bytes), fullname
		} else if !os.IsNotExist(err) {
			log.Fatalln(err)
		}
	}
	log.Fatalf(
		"could not find %s in any of the source paths:\n\t%s",
		filename, strings.Join(paths, "\n\t"),
	)
	return "", ""
}

func lexFile(filename string, paths []string) *lexer {
	bytes, fullname := readFirstFromPaths(filename, paths)
	log.SetFlags(0)
	log.SetPrefix(filename + ": ")
	l := &lexer{
		stream: *newLexStream(bytes),
		items:  make(chan item),
		paths:  append(paths, filepath.Dir(fullname)),
	}
	go l.run()
	return l
}

// checkMinParams checks if the item has at least min parameters and prints a
// log message if it doesn't.
func (i *item) checkMinParams(min int) bool {
	if given := len(i.params); given < min {
		log.Printf(
			"%s requires at least %d parameters, %d given\n",
			strings.ToUpper(i.val), min, given,
		)
		return false
	}
	return true
}

func (i item) String() string {
	var ret string
	switch i.typ {
	case itemLabel:
		ret = i.sym + ":\n"
	case itemInstruction:
		if i.sym != "" {
			ret = i.sym
		}
		ret += "\t" + i.val
	}
	if len(i.params) > 0 {
		ret += "\t" + strings.Join(i.params, ", ")
	}
	if i.typ == itemInstruction {
		ret += "\n"
	}
	return ret
}

func main() {
	filename := kingpin.Arg(
		"filename", "Assembly file.",
	).Required().ExistingFile()

	syntax := kingpin.Flag(
		"syntax", "Target assembler.",
	).Default("TASM").Enum("TASM", "MASM")

	includes := kingpin.Flag(
		"include", "Add the given directory to the list of assembly include directories.",
	).Default(".").Short('I').Strings()

	kingpin.Parse()

	l := lexFile(*filename, *includes)
	p := parser{syntax: *syntax}

	for i := range l.items {
		p.eval(&i)
	}
	for _, i := range p.instructions {
		fmt.Print(i)
	}
	p.end()
}
