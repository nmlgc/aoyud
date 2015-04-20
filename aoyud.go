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
	"os"
	"path/filepath"
	"strings"
)

type SourcePos struct {
	filename *string
	line     uint // 0 = EOF
}

func (p SourcePos) String() string {
	if p.line == 0 {
		return fmt.Sprintf("%s(EOF):", *p.filename)
	} else {
		return fmt.Sprintf("%s(%d):", *p.filename, p.line)
	}
}

type ItemPos []SourcePos

func (p *ItemPos) String() string {
	ret := ""
	if p == nil {
		return ret
	}
	for i, pos := range *p {
		if i != 0 {
			ret += "\n" + strings.Repeat(" ", i)
		}
		ret += pos.String()
	}
	return ret + " "
}

func NewItemPos(filename *string, line uint) *ItemPos {
	return &ItemPos{SourcePos{filename: filename, line: line}}
}

type itemParams []string

func (p itemParams) String() string {
	return strings.Join(p, ", ")
}

// item represents a token or text string returned from the scanner.
type item struct {
	pos    ItemPos    // Code position of this item and the macros it came from.
	typ    itemType   // The type of this item
	sym    string     // Optional symbol name
	val    string     // Name of the instruction or label. Limited to ASCII characters.
	params itemParams // Instruction parameters
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError       itemType = iota // error occurred; value is text of error
	itemLabel                       // jump target
	itemInstruction                 // instruction or directive and its parameters
)

// Range defines a range of numbers. Negative values for Max indicate no upper
// limit.
type Range struct {
	Min, Max int
}

// checkParamRange returns nil if the number of parameters in the item is
// within the given range, or an error message if it isn't.
func (it *item) checkParamRange(r Range) *ErrorList {
	var sev ErrorSeverity
	given := len(it.params)
	below := given < r.Min
	if below || uint(given) > uint(r.Max) {
		var textErr, textParams string
		if below {
			if given > 0 {
				textParams = ": " + it.params.String()
			}
			textErr = fmt.Sprintf(
				"requires at least %d parameters, %d given", r.Min, given,
			) + textParams
			sev = ESError
		} else {
			if r.Max == 0 {
				textParams = "accepts no parameters"
			} else {
				textParams = fmt.Sprintf("accepts a maximum of %d parameters", r.Max)
			}
			extra := given - r.Max
			textErr = textParams + fmt.Sprintf(
				", ignoring %d additional ones: ", extra,
			) + strings.Join(it.params[given-extra:], ", ")
			sev = ESWarning
		}
		return ErrorListF(sev, "%s %s", it.val, textErr)
	}
	return nil
}

type parseFile struct {
	stream lexStream
	name   *string
	paths  []string   // search paths for relative includes
	prev   *parseFile // file that included this one, or nil for the main file
}

func INCLUDE(p *parser, it *item) *ErrorList {
	return p.StepIntoFile(it.params[0], p.file.paths)
}

// lexItem scans and returns the next item from the currently parsed file. All
// errors in the returned list are already assigned to their correct position.
func (p *parser) lexItem() (ret *item, err *ErrorList) {
	var secondRule SymRule
	var val asmVal

	first := p.file.stream.nextUntil(&insDelim)
	pos := NewItemPos(p.file.name, p.file.stream.line)
	p.file.stream.ignore(&whitespace)

	// Handle one-char instructions
	switch p.file.stream.peek() {
	// Label?
	case ':':
		p.file.stream.next()
		return &item{pos: *pos, typ: itemLabel, sym: first}, nil
	// Assignment? (Needs to be a special case because = doesn't need to be
	// surrounded by spaces, and nextUntil() isn't designed to handle that.)
	case '=':
		p.file.stream.next()
		ret := &item{pos: *pos, typ: itemInstruction, sym: first, val: "="}
		return p.lexParam(ret), nil
	}

	second := p.file.stream.peekUntil(&insDelim)
	firstUpper := strings.ToUpper(first)
	secondUpper := strings.ToUpper(second)
	if _, ok := Keywords[firstUpper]; ok {
		first = firstUpper
	} else if k, ok := Keywords[secondUpper]; ok {
		second = secondUpper
		secondRule = k.Sym
	} else if val, err = p.syms.Lookup(first); val != nil {
		switch val.(type) {
		case asmExpression:
			// TODO: Well, "expressions" can be anything, both syntactically
			// valid and invalid…
		case asmStruc:
		case asmDataPtr: // These can be redefined with an identical value.
		case asmMacro:
			break
		default:
			err = err.AddFAt(pos, ESError,
				"%s not allowed here: %s", val.Thing(), first,
			)
		}
	} else if val, err = p.syms.Lookup(second); val != nil {
		switch val.(type) {
		case asmStruc:
			secondRule = Optional
		}
	}

	if firstUpper == "COMMENT" {
		delim := charGroup{p.file.stream.next()}
		p.file.stream.nextUntil(&delim)
		p.file.stream.nextUntil(&linebreak) // Yes, everything else on the line is ignored.
		return nil, nil
	} else if secondRule != NotAllowed {
		ret = &item{pos: *pos, typ: itemInstruction, sym: first, val: second}
		p.file.stream.nextUntil(&insDelim)
	} else if len(first) > 0 {
		ret = &item{pos: *pos, typ: itemInstruction, val: first}
	}
	return p.lexParam(ret), err
}

// lexParam recursively scans the parameters following the given item and adds
// them to it.
func (p *parser) lexParam(it *item) *item {
	if it != nil {
		if param := p.file.stream.nextParam(); len(param) > 0 {
			it.params = append(it.params, param)
		}
	}
	switch p.file.stream.next() {
	case ';', '\\':
		// Comment
		p.file.stream.nextUntil(&linebreak)
	case '\r', '\n':
		p.file.stream.ignore(&linebreak)
	case eof:
		p.file = p.file.prev
	default:
		return p.lexParam(it)
	}
	return it
}

// readFirstFromPaths reads and returns the contents of a file with name
// filename from the first directory in the given list that contains such a
// file, the full path to the file that was read, as well as any error that
// occurred.
func readFirstFromPaths(filename string, paths []string) (string, string, *ErrorList) {
	for _, path := range paths {
		fullname := filepath.Join(path, filename)
		bytes, err := ioutil.ReadFile(fullname)
		if err == nil {
			return string(bytes), fullname, nil
		} else if !os.IsNotExist(err) {
			return "", "", NewErrorList(ESFatal, err)
		}
	}
	return "", "", ErrorListF(ESFatal,
		"could not find %s in any of the source paths:\n\t%s",
		filename, strings.Join(paths, "\n\t"),
	)
}

func (p *parser) StepIntoFile(filename string, paths []string) *ErrorList {
	bytes, fullname, err := readFirstFromPaths(filename, paths)
	if err == nil {
		p.file = &parseFile{
			name:   &filename,
			stream: *newLexStream(bytes),
			paths:  append(paths, filepath.Dir(fullname)),
			prev:   p.file,
		}
	}
	return err
}

func (it item) String() string {
	var ret string
	switch it.typ {
	case itemLabel:
		ret = it.sym + ":"
	case itemInstruction:
		if it.sym != "" {
			ret = it.sym
		}
		ret += "\t" + it.val
	}
	if len(it.params) > 0 {
		ret += "\t" + it.params.String()
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

	p, err := Parse(*filename, *syntax, *includes)
	err.Print()

	for _, i := range p.instructions {
		fmt.Println(i)
	}
	ErrorListF(ESDebug, "%s", p.syms).Print()
}
