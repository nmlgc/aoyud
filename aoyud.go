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

func (p ItemPos) String() string {
	ret := ""
	for i, pos := range p {
		if i != 0 {
			ret += "\n" + strings.Repeat(" ", i)
		}
		ret += pos.String()
	}
	return ret + " "
}

func NewItemPos(filename *string, line uint) ItemPos {
	return ItemPos{SourcePos{filename: filename, line: line}}
}

type itemParams []string

func (p itemParams) String() string {
	return strings.Join(p, ", ")
}

// item represents a token or text string returned from the scanner.
type item struct {
	num    int        // # of this item within the entire code; filled in by the parser.
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
func (it *item) checkParamRange(r Range) ErrorList {
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

func INCLUDE(p *parser, it *item) ErrorList {
	return p.StepIntoFile(it.params[0], p.file.paths)
}

// lexItem scans and returns the next item from the given stream, or nil if
// the end of the stream has been reached. All errors in the returned list are
// already assigned to their correct position.
func (p *parser) lexItem(stream *lexStream) (ret *item, err ErrorList) {
	var secondRule SymRule
	var pos ItemPos
	context := KeywordType(0)

	first := stream.nextUntil(insDelim)
	pos = append(pos, stream.pos...)
	stream.ignore(whitespace)

	// Handle one-char instructions
	switch stream.peek() {
	// Label?
	case ':':
		stream.next()
		return &item{pos: pos, typ: itemLabel, sym: first}, nil
	// Assignment? (Needs to be a special case because = doesn't need to be
	// surrounded by spaces, and nextUntil() isn't designed to handle that.)
	case '=':
		stream.next()
		ret := &item{pos: pos, typ: itemInstruction, sym: first, val: "="}
		return p.lexParam(stream, context, ret, err)
	}

	second := stream.peekUntil(insDelim)
	firstUpper := strings.ToUpper(first)
	secondUpper := strings.ToUpper(second)
	if k, ok := Keywords[firstUpper]; ok {
		first = firstUpper
		context = k.Type
	} else if k, ok := Keywords[secondUpper]; ok {
		second = secondUpper
		context = k.Type
		secondRule = k.Sym
	} else if val, errLookup := p.syms.Lookup(first); val != nil {
		err = err.AddLAt(pos, errLookup)
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
	} else if val, errLookup := p.syms.Lookup(second); val != nil {
		err = err.AddLAt(pos, errLookup)
		switch val.(type) {
		case asmStruc:
			context |= SingleParam
			secondRule = Optional
		}
	}

	if firstUpper == "COMMENT" {
		delim := charGroup{stream.next()}
		stream.nextUntil(delim)
		stream.nextUntil(linebreak) // Yes, everything else on the line is ignored.
		return p.lexItem(stream)
	} else if secondRule != NotAllowed {
		ret = &item{pos: pos, typ: itemInstruction, sym: first, val: second}
		stream.nextUntil(insDelim)
	} else if len(first) > 0 {
		ret = &item{pos: pos, typ: itemInstruction, val: first}
	}
	return p.lexParam(stream, context, ret, err)
}

// lexParam recursively scans the parameters following the given item from the
// given stream and adds them to it.
func (p *parser) lexParam(stream *lexStream, context KeywordType, it *item, err ErrorList) (*item, ErrorList) {
	if it != nil {
		if param := stream.nextParam(context); len(param) > 0 {
			it.params = append(it.params, param)
		}
	}
	switch stream.next() {
	case ';', '\\':
		// Comment
		stream.nextUntil(linebreak)
	case '\r', '\n':
		stream.ignore(linebreak)
	case eof:
		return it, err
	default:
		return p.lexParam(stream, context, it, err)
	}
	if it == nil {
		return p.lexItem(stream)
	}
	return it, err
}

// readFirstFromPaths reads and returns the contents of a file with name
// filename from the first directory in the given list that contains such a
// file, the full path to the file that was read, as well as any error that
// occurred.
func readFirstFromPaths(filename string, paths []string) (string, string, ErrorList) {
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

func (p *parser) StepIntoFile(filename string, paths []string) ErrorList {
	bytes, fullname, err := readFirstFromPaths(filename, paths)
	if err == nil {
		p.file = &parseFile{
			stream: *NewLexStream(&filename, bytes),
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
	ErrorListFAt(NewItemPos(filename, 0), ESDebug,
		"Symbols: [\n%s\n]", p.syms,
	).Print()

	for _, sym := range p.syms.Map {
		switch sym.Val.(type) {
		case *asmSegment:
			seg := sym.Val.(*asmSegment)
			if len(seg.chunks) == 1 && len(seg.chunks[0]) > 0 {
				dumpfile := *filename + "." + seg.Name() + ".bin"
				ioutil.WriteFile(dumpfile, seg.chunks[0].Emit(), os.ModePerm)
			}
		}
	}
}
