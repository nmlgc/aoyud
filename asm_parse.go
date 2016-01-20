// Assembly syntax parser.

package main

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"
)

type Thingy interface {
	Thing() string // Returns a singular noun describing this type of value
}

type asmVal interface {
	Thingy
	fmt.Stringer
}

type Nestable interface {
	// Returns "open <type of block>".
	OpenThing() string
	// Returns "open <types of block>".
	OpenThings() string
	// Returns a friendly name of the current block.
	Name() string
}

// asmInt represents an integer that will be output in a defined base.
type asmInt struct {
	n        int64  // The value itself
	ptr      uint64 // Nonzero values turn the integer into a pointer of this length
	base     uint8
	wordsize uint8 // Number of bytes to be produced on Emit()
}

func (v asmInt) Thing() string {
	return "integer constant"
}

func (v asmInt) width() uint {
	n := v.n
	if n < 0 {
		n = -n
	}
	if n < 0xFF {
		return 1
	} else if n < 0xFFFF {
		return 2
	} else if n < 0xFFFFFFFF {
		return 4
	}
	return 8
}

func (v asmInt) String() string {
	var ret string
	if v.base == 0 {
		v.base = 10
	}
	if v.base <= 16 {
		ret = strconv.FormatInt(v.n, int(v.base))
		switch v.base {
		case 2:
			ret += "b"
		case 8:
			ret += "o"
		case 16:
			start := 0
			if ret[0] == '-' || ret[0] == '+' {
				start++
			}
			if ret[start] >= 'a' && ret[start] <= 'f' {
				ret = ret[:start] + "0" + ret[start:]
			}
			ret += "h"
		}
	} else if v.base == 255 {
		ret = quoteASCII(v.formatASCII())
	}
	if v.ptr != 0 {
		ret = "(" + strconv.FormatUint(v.ptr, 10) + "*) " + ret
	}
	return ret
}

func (v asmInt) Emit() []byte {
	ret := make([]byte, v.wordsize)
	rest := v.n
	for i := uint8(0); i < v.wordsize; i++ {
		ret[v.wordsize-1-i] = byte(rest & 0xFF)
		rest >>= 8
	}
	return ret
}

func (v asmInt) Len() uint {
	return uint(v.wordsize)
}

// FitsIn returns whether n can fit in the given number of bytes.
func (v asmInt) FitsIn(bytes uint) bool {
	// In fact, 64-bit declarations in JWasm don't limit the value at all.
	if bytes >= 8 {
		return true
	}
	return v.n >= -int64(1<<(bytes*8)) &&
		v.n <= int64((1<<(bytes*8)-1))
}

// isAsmInt checks whether input is to be interpreted as a single integer
// constant.
func isAsmInt(input string) bool {
	if len(input) == 0 {
		return false
	}
	f := input[0]
	validFirst := (f >= '0' && f <= '9')
	return validFirst && (strings.IndexAny(input, " \t") == -1)
}

// newAsmInt parses the input as an integer constant.
func newAsmInt(input string) (asmInt, ErrorList) {
	length := len(input)
	base := uint8(0)
	switch unicode.ToLower(rune(input[length-1])) {
	case 'b':
		base = 2
	case 'o', 'q':
		base = 8
	case 't': // MASM only
		base = 10
	case 'h':
		base = 16
	}
	if base != 0 {
		input = input[:length-1]
	} else {
		base = 10
	}
	n, err := strconv.ParseInt(input, int(base), 0)
	if err != nil {
		return asmInt{}, NewErrorList(ESError, err)
	}
	return asmInt{n: n, base: base}, nil
}

// asmExpression represents an evaluable expression string.
type asmExpression string

func (v asmExpression) Thing() string {
	return "arithmetic expression"
}

func (v asmExpression) String() string {
	return "(" + string(v) + ")"
}

type asmMacroArg struct {
	name string
	typ  string
	def  string
}

func (v asmMacroArg) String() string {
	ret := v.name
	if v.typ != "" {
		ret += ":" + v.typ
		if v.typ == "=" {
			ret += "<" + v.def + ">"
		}
	}
	return ret
}

type asmMacro struct {
	args   []asmMacroArg
	code   []item
	locals itemParams
}

func (v asmMacro) Thing() string {
	return "multiline macro"
}

func (v asmMacro) String() string {
	ret := "MACRO"
	for i, arg := range v.args {
		if i != 0 {
			ret += ", "
		} else {
			ret += "\t"
		}
		ret += arg.String()
	}
	if len(v.locals) != 0 {
		ret += "\n\tLOCAL\t" + v.locals.String()
	}
	ret += "\n"
	for _, ins := range v.code {
		ret += ins.String() + "\n"
	}
	return ret + "\tENDM"
}

// newMacro creates a new multiline macro ending at itemNum.
func (p *parser) newMacro(itemNum int) (ret asmMacro, err ErrorList) {
	header := p.instructions[p.macro.start]
	args := make([]asmMacroArg, len(header.params))
	for i := range header.params {
		nameOrg, typOrg := splitColon(header.params[i])
		args[i].name = p.syms.ToSymCase(nameOrg)
		args[i].typ = strings.ToUpper(typOrg)
		// Verify types
		if args[i].typ == "REST" || args[i].typ == "VARARG" {
			if i != len(header.params)-1 {
				// TASM would actually accept this, but we better
				// complain since it doesn't make sense at all.
				return asmMacro{}, ErrorListFAt(header.pos, ESError,
					"%s:%s must be the last parameter",
					args[i].name, args[i].typ,
				)
			}
		} else if !(args[i].typ == "" || args[i].typ == "REQ") {
			if typOrg[0] == '=' {
				def, err := p.text(strings.TrimSpace(typOrg[1:]))
				if err.Severity() >= ESError {
					return asmMacro{}, err
				}
				args[i].typ = "="
				args[i].def = def
			} else {
				err = err.AddFAt(header.pos, ESWarning,
					"invalid macro argument type: %s", args[i].typ,
				)
			}
		}
	}
	var locals []string
	localsAllowed := true
	code := p.instructions[p.macro.start+1 : itemNum]
	for i := 0; i < len(code); i++ {
		if strings.EqualFold(code[i].val, "LOCAL") {
			if localsAllowed {
				for _, param := range code[i].params {
					locals = append(locals, p.syms.ToSymCase(param))
				}
				code = code[i+1:]
				i--
			} else {
				err = err.AddFAt(code[i].pos, ESError,
					"LOCAL directives must come first in a macro body, ignoring: %s",
					code[i].params.String(),
				)
			}
		} else {
			localsAllowed = false
		}
	}
	return asmMacro{args, code, locals}, err
}

// expandMacro expands the multiline macro m using the parameters of it and
// calls p.evalNew for every line in the macro. Returns false if the expansion
// was successful, true otherwise.
func (p *parser) expandMacro(m asmMacro, it *item) (bool, ErrorList) {
	var errList ErrorList
	replaceMap := make(map[string]string)

	setArg := func(name string, i int) (bool, ErrorList) {
		var text string
		var err ErrorList
		ret := len(it.params) > i && len(it.params[i]) > 0
		if ret {
			if it.params[i][0] == '<' || it.params[i][0] == '%' {
				text, err = p.text(it.params[i])
				if err.Severity() >= ESError {
					return false, err
				}
				replaceMap[name] = text
			} else {
				replaceMap[name] = it.params[i]
			}
		}
		return ret, err
	}

	replace := func(it *item, s string) string {
		ret := ""
		andCached := false
		for stream := NewLexStreamAt(it.pos, s); stream.peek() != eof; {
			// Be sure to copy any whitespace in s.
			start := stream.c
			stream.ignore(whitespace)
			ret += s[start:stream.c]

			token := stream.nextToken(macroDelim)
			if token == "&" {
				andCached = true
				token = ""
			} else if arg, ok := replaceMap[p.syms.ToSymCase(token)]; ok {
				token = arg
				if stream.peek() == '&' {
					stream.next()
				}
				andCached = false
			} else if andCached {
				ret += "&"
				andCached = false
			}
			ret += token
		}
		return ret
	}

	for i, arg := range m.args {
		var got bool
		if arg.typ == "REST" || arg.typ == "VARARG" {
			replaceMap[arg.name] = it.params[i:].String()
		} else {
			var err ErrorList
			replaceMap[arg.name] = arg.def
			got, err = setArg(arg.name, i)
			errList = errList.AddL(err)
		}
		if arg.typ == "REQ" && !got {
			errList = errList.AddF(ESError,
				"macro argument #%d (%s) is required", i+1, arg.name,
			)
		}
	}
	if errList.Severity() >= ESError {
		return true, errList
	}
	for _, local := range m.locals {
		// Who knows, some code might actually rely on the resulting
		// labels being named exactly like this.
		replaceMap[local] = fmt.Sprintf("??%04X", p.macroLocalCount)
		p.macroLocalCount++
	}
	for i := range m.code {
		line := replace(&m.code[i], m.code[i].String())
		stream := NewLexStreamAt(it.pos, line)
		stream.pos = append(stream.pos, m.code[i].pos...)
		expanded, err := p.lexItem(stream)
		errList = errList.AddL(err)
		if err.Severity() < ESError {
			expanded.num = len(p.instructions)
			errList = errList.AddLAt(expanded.pos, p.evalNew(expanded))
		}
	}
	return false, errList
}

// NestInfo represents a type of named block that can be nested.
type NestInfo struct {
	name  string // Name of level 1
	start int    // First item in the instruction list that belongs to level 1
	nest  int    // Current nesting level
}

// ErrorListOpen returns an "open block" error list for block and all previous
// nested blocks.
func ErrorListOpen(nest []Nestable) ErrorList {
	if len(nest) == 0 {
		return nil
	}
	str := nest[0].OpenThing() + ": "
	if len(nest) >= 2 {
		str = nest[0].OpenThings() + ": "
	}
	str += nest[len(nest)-1].Name()
	for i := len(nest) - 2; i >= 0; i-- {
		str += " ← " + nest[i].Name()
	}
	return ErrorListF(ESWarning, str)
}

type parser struct {
	instructions []item
	// General state
	pass2           bool
	file            *parseFile
	syntax          string
	syms            SymMap
	intSyms         InternalSyms
	caseSensitive   bool
	macroLocalCount int    // Number of LOCAL directives expanded
	segCodeName     string // Name of the segment entered with .CODE
	segDataName     string // Name of the segment entered with .DATA
	// Open blocks
	proc   NestInfo
	macro  NestInfo
	strucs []Nestable
	segs   []Nestable
	// Conditionals
	ifNest  int  // IF nesting level
	ifMatch int  // Last IF nesting level that evaluated to true
	ifElse  bool // Can the current level still have an ELSE* block?
}

func splitColon(s string) (string, string) {
	var key, val string
	split := strings.SplitN(s, ":", 2)
	key = strings.TrimSpace(split[0])
	if len(split) > 1 {
		val = strings.TrimSpace(split[1])
	}
	return key, val
}

func (it *item) missingRequiredSym() ErrorList {
	if it.sym == "" {
		return ErrorListF(ESError, "%s needs a name", it.val)
	}
	return nil
}

func (it *item) checkSyntaxFor(k Keyword) ErrorList {
	if k.Sym == Mandatory {
		if err := it.missingRequiredSym(); err != nil {
			return err
		}
	}
	return it.checkParamRange(k.ParamRange)
}

func PROC(p *parser, it *item) (err ErrorList) {
	if p.proc.nest == 0 {
		p.proc.name = it.sym
		p.proc.start = it.num
	} else {
		err = ErrorListF(ESWarning, "ignoring nested procedure %s", it.sym)
	}
	p.proc.nest++
	return err
}

func ENDP(p *parser, it *item) (err ErrorList) {
	if p.proc.nest == 0 {
		return ErrorListF(ESDebug,
			"ignoring procedure without a PROC directive: %s", it.sym,
		)
	} else if p.proc.nest == 1 {
		err = ErrorListF(ESDebug,
			"found procedure %s ranging from lex items #%d-#%d",
			p.proc.name, p.proc.start, it.num,
		)
	}
	p.proc.nest--
	return err
}

func MODEL(p *parser, it *item) (err ErrorList) {
	type modelVals struct {
		model, codesize, datasize uint8
	}
	type modifierMap map[string]func() ErrorList
	type modifiers struct {
		typ  string
		m    modifierMap
		prev *string
	}

	// Parse results
	model := MemoryModel(0)
	farstack := false
	showNearstackWarning := false
	thirtytwo := uint8(0)
	language := uint8(0)
	codesegname := ""
	datasegname := ""

	parseStack := func(far bool) (err ErrorList) {
		if model&Flat != 0 && showNearstackWarning && (!far || !farstack) {
			err = err.AddF(ESWarning,
				"NEARSTACK is ignored for flat memory models",
			)
			far = true
			showNearstackWarning = false
		}
		if !far {
			showNearstackWarning = true
		}
		farstack = far
		return err
	}

	modelMap := map[string]struct {
		vals  modelVals
		model MemoryModel
	}{
		"TINY":    {modelVals{1, 0, 0}, Tiny},
		"SMALL":   {modelVals{2, 0, 0}, Small},
		"COMPACT": {modelVals{3, 0, 1}, Compact},
		"MEDIUM":  {modelVals{4, 1, 0}, Medium},
		"LARGE":   {modelVals{5, 1, 1}, Large},
		"HUGE":    {modelVals{6, 1, 2}, Huge},
		"TPASCAL": {modelVals{0, 0, 1}, TPascal},
		"TCHUGE":  {modelVals{7, 1, 2}, TCHuge},
		// Yes, the TASM manual actually got @Model wrong.
		// For MASM, @Model is changed to 7.
		"FLAT": {modelVals{1, 0, 0}, Flat},
	}
	masmFlat := modelVals{7, 0, 0}

	// interfaces defines values for the @Interface symbol.
	interfaces := modifiers{typ: "language", m: modifierMap{
		"NOLANGUAGE": func() ErrorList { language = 0; return nil },
		"C":          func() ErrorList { language = 1; return nil },
		"SYSCALL":    func() ErrorList { language = 2; return nil },
		"STDCALL":    func() ErrorList { language = 3; return nil },
		"PASCAL":     func() ErrorList { language = 4; return nil },
		"FORTRAN":    func() ErrorList { language = 5; return nil },
		"BASIC":      func() ErrorList { language = 6; return nil },
		"FASTCALL":   func() ErrorList { language = 7; return nil }, // MASM only
		"PROLOG":     func() ErrorList { language = 7; return nil },
		"CPP":        func() ErrorList { language = 8; return nil },
	}}
	languageModifiers := modifiers{typ: "language modifier", m: modifierMap{
		"NORMAL":  func() ErrorList { return nil },
		"WINDOWS": func() ErrorList { return nil },
		"ODDNEAR": func() ErrorList { return nil },
		"ODDFAR":  func() ErrorList { return nil },
	}}
	tasmModelModifiers := modifiers{typ: "model modifier", m: modifierMap{
		"NEARSTACK": func() ErrorList { return parseStack(false) },
		"FARSTACK":  func() ErrorList { return parseStack(true) },
		"DOS":       func() ErrorList { return nil },
		"OS2":       func() ErrorList { return nil },
		"NT":        func() ErrorList { return nil },
		"OS_DOS":    func() ErrorList { return nil },
		"OS_OS2":    func() ErrorList { return nil },
		"OS_NT":     func() ErrorList { return nil },
		"USE16":     func() ErrorList { thirtytwo = 0; return nil },
		"USE32": func() ErrorList {
			if p.intSyms.CPU&cpu386 == 0 {
				return ErrorListF(ESError,
					"32-bit segments require at least a .386 CPU setting: USE32",
				)
			}
			thirtytwo = 1
			return nil
		},
	}}
	masmStackDistance := modifiers{typ: "stack distance", m: modifierMap{
		"NEARSTACK": func() ErrorList { return parseStack(false) },
		"FARSTACK":  func() ErrorList { return parseStack(true) },
	}}
	masmOS := modifiers{typ: "OS", m: modifierMap{
		"OS_DOS": func() ErrorList { return nil },
		"OS_OS2": func() ErrorList { return nil },
	}}

	tasmParseModifier := func(param string, mods modifiers) {
		if mod, ok := mods.m[param]; ok {
			err = err.AddL(mod())
		} else {
			err = err.AddF(ESError, "invalid %s: %s", mods.typ, param)
		}
	}

	masmParseModifier := func(param string, mods *modifiers) bool {
		mod, ok := mods.m[param]
		if !ok {
			return ok
		} else if mods.prev != nil {
			err = err.AddF(ESWarning,
				"%s already specified as %s, ignoring: %s",
				mods.typ, *mods.prev, param,
			)
		}
		mods.prev = &param
		err = err.AddL(mod())
		return true
	}

	parseModel := func(modelstr string) (err ErrorList) {
		if modelstr == "" {
			return err.AddF(ESError, "no memory model given: %s", it.params)
		}
		modelstr = strings.ToUpper(modelstr)
		if mod, ok := modelMap[modelstr]; ok {
			model = mod.model
			p.intSyms.SymModel = &mod.vals.model
			p.intSyms.SymCodeSize = &mod.vals.codesize
			p.intSyms.SymDataSize = &mod.vals.datasize
			p.intSyms.Model = &mod.model
			if model == Flat {
				if p.intSyms.CPU&cpu386 == 0 {
					return err.AddF(ESError,
						"FLAT model requires at least a .386 CPU",
					)
				} else if p.syntax == "MASM" {
					p.intSyms.SymModel = &masmFlat.model
				}
			}
			if mod.model&Flat != 0 {
				err = err.AddL(parseStack(true))
			}
		} else {
			err = err.AddF(ESError, "invalid memory model: %s", modelstr)
		}
		return err
	}

	getSegName := func(curname, suffix string, filenamecond bool) string {
		if curname == "" {
			if filenamecond {
				curname += string(p.intSyms.FileName)
			}
			curname += suffix
		}
		return curname
	}

	if p.syntax == "TASM" {
		// Optional model modifier
		modelStream := NewLexStreamAt(it.pos, it.params[0])
		modelstr := strings.ToUpper(modelStream.nextUntil(whitespace))

		// Yup, reading multiple ones until a valid model name.
		for modelStream.peek() != eof {
			tasmParseModifier(modelstr, tasmModelModifiers)
			modelstr = strings.ToUpper(modelStream.nextUntil(whitespace))
			if _, ok := modelMap[modelstr]; ok {
				break
			}
		}
		if err.Severity() >= ESError {
			return err
		}

		// Model
		err = err.AddL(parseModel(modelstr))
		if err.Severity() >= ESError {
			return err
		}

		// Optional code segment name
		codesegname = modelStream.nextUntil(whitespace)
		if codesegname != "" && model&FarCode == 0 {
			err = err.AddF(ESWarning,
				"code segment name ignored for near-code models: %s",
				codesegname,
			)
			codesegname = ""
		}

		// Optional data segment name for TCHUGE. Sort of documented, actually.
		datasegname = modelStream.nextUntil(whitespace)
		if datasegname != "" && modelstr != "TCHUGE" {
			err = err.AddF(ESWarning,
				"data segment name may only be specified for the TCHUGE model: %s",
				datasegname,
			)
			datasegname = ""
		}

		if modelStream.peek() != eof {
			err = err.AddF(ESWarning,
				"ignoring garbage at the end of the first parameter: %s",
				modelStream.input[modelStream.c+1:],
			)
		}

		// Language
		if len(it.params) > 1 {
			languageStream := NewLexStreamAt(it.pos, it.params[1])
			word := strings.ToUpper(languageStream.nextUntil(whitespace))
			if languageStream.peek() != eof { // Yup, reading only one
				tasmParseModifier(word, languageModifiers)
				word = strings.ToUpper(languageStream.nextUntil(whitespace))
			}
			tasmParseModifier(word, interfaces)
		}

		// One optional model modifier
		if len(it.params) > 2 {
			param := strings.ToUpper(it.params[2])
			tasmParseModifier(param, tasmModelModifiers)
		}

		p.intSyms.ThirtyTwo = &thirtytwo

		// TASM's syntax accepts 3 comma-separated parameters as opposed to
		// MASM's maximum of 4, so we explicitly check the range again.
		err = err.AddL(it.checkParamRange(Range{1, 3}))
	} else {
		err = err.AddL(parseModel(it.params[0]))
		if err.Severity() >= ESError {
			return err
		}
		for _, param := range it.params[1:] {
			param = strings.ToUpper(param)
			if masmParseModifier(param, &masmStackDistance) {
			} else if masmParseModifier(param, &masmOS) {
			} else if masmParseModifier(param, &interfaces) {
			} else {
				return err.AddF(ESError, "invalid model modifier: %s", param)
			}
		}
	}
	p.intSyms.Interface = &language

	// TASM 5.0 actually doesn't even set @STACK for the TCHUGE model.
	// Certainly a bug.
	var stackgroup asmExpression
	if farstack {
		stackgroup = asmExpression("STACK")
	} else {
		stackgroup = asmExpression("DGROUP")
	}
	p.intSyms.StackGroup = &stackgroup

	// Initialize default segments.
	p.segCodeName = getSegName(codesegname, "_TEXT", model&FarCode != 0)
	p.segDataName = getSegName(datasegname, "_DATA", model == TCHuge)
	_, errCS := p.GetSegment(p.segCodeName, model == Tiny)
	_, errDS := p.GetSegment(p.segDataName, true)
	err = err.AddL(errCS)
	err = err.AddL(errDS)
	return err
}

func EQUALS(p *parser, it *item) ErrorList {
	ret, err := p.syms.evalInt(it.pos, it.params[0])
	if err.Severity() < ESError {
		return p.syms.Set(it.sym, *ret, false)
	}
	return err
}

func EQU(p *parser, it *item) (err ErrorList) {
	var existing asmVal
	tryNumber := true
	if existing, err = p.syms.Lookup(it.sym); existing != nil {
		switch existing.(type) {
		case asmInt:
			tryNumber = true
		default:
			tryNumber = false
		}
	}
	if tryNumber {
		number, numberErr := p.syms.evalInt(it.pos, it.params[0])
		if numberErr.Severity() < ESError {
			err = err.AddL(numberErr)
			return err.AddL(p.syms.Set(it.sym, *number, true))
		}
	}
	return p.syms.Set(it.sym, asmExpression(it.params[0]), false)
}

// text evaluates s as a text string used in a conditional directive.
func (p *parser) text(s string) (string, ErrorList) {
	fail := func() (string, ErrorList) {
		return "", ErrorListF(ESError,
			"invalid <text string> or %%text_macro: %s", s,
		)
	}
	if s[0] == '<' {
		var err ErrorList
		s = s[1:]
		// TASM does not strip whitespace here, JWasm does.
		if p.syntax == "MASM" {
			s = strings.TrimSpace(s)
		}
		rb := strings.IndexByte(s, '>')
		if rb == -1 {
			return fail()
		} else if rb != len(s)-1 {
			err = ErrorListF(ESWarning,
				"extra characters on line: %s", s[rb+1:],
			)
		}
		return s[:rb], err
	} else if s[0] == '%' {
		name := strings.TrimSpace(s[1:])
		sym, err := p.syms.Get(name)
		if err != nil {
			return "", err
		}
		switch sym.(type) {
		case asmInt:
			return strconv.FormatInt(sym.(asmInt).n, 10), nil
		case asmExpression:
			return string(sym.(asmExpression)), nil
		default:
			return "", ErrorListF(ESError,
				"can't use %s as a text string: %s", sym.Thing(), name,
			)
		}
	}
	return fail()
}

func (p *parser) isBlank(s string) (bool, ErrorList) {
	ret, err := p.text(s)
	return len(ret) == 0, err
}

func (p *parser) isEqual(s1, s2 string) (bool, ErrorList) {
	ret1, err1 := p.text(s1)
	ret2, err2 := p.text(s2)
	return ret1 == ret2, err1.AddL(err2)
}

func (p *parser) isEqualFold(s1, s2 string) (bool, ErrorList) {
	ret1, err1 := p.text(s1)
	ret2, err2 := p.text(s2)
	return strings.EqualFold(ret1, ret2), err1.AddL(err2)
}

func (p *parser) evalIf(match bool) ErrorList {
	valid := match && p.ifMatch == p.ifNest
	if valid {
		p.ifMatch++
	}
	p.ifNest++
	p.ifElse = !valid
	return nil
}

func (p *parser) evalElseif(directive string, match bool) ErrorList {
	if p.ifNest == 0 {
		return ErrorListF(ESWarning, "unmatched %s", directive)
	}
	if p.ifMatch == p.ifNest {
		p.ifMatch--
	} else if p.ifMatch == (p.ifNest-1) && p.ifElse && match {
		p.ifMatch++
		p.ifElse = false
	}
	return nil
}

type ifidnMode struct {
	compareFn func(*parser, string, string) (bool, ErrorList)
	identical bool
}

// ifidnModeMap abstracts away the differences between IFIDN(I) and IFDIF(I),
// so that all four can be implemented in a single function.
var ifidnModeMap = map[string]ifidnMode{
	"IFIDN":  {compareFn: (*parser).isEqual, identical: true},
	"IFIDNI": {compareFn: (*parser).isEqualFold, identical: true},
	"IFDIF":  {compareFn: (*parser).isEqual, identical: false},
	"IFDIFI": {compareFn: (*parser).isEqualFold, identical: false},
}

func IFDEF(p *parser, it *item) ErrorList {
	mode := it.val == "IFDEF"
	val, err := p.syms.Lookup(it.params[0])
	return err.AddL(p.evalIf((val != nil) == mode))
}

func IF(p *parser, it *item) ErrorList {
	mode := it.val == "IF"
	ret, err := p.syms.evalBool(it.pos, it.params[0])
	return err.AddL(p.evalIf(ret == mode))
}

func IFB(p *parser, it *item) ErrorList {
	mode := it.val == "IFB"
	ret, err := p.isBlank(it.params[0])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalIf(ret == mode)
}

func IFIDN(p *parser, it *item) ErrorList {
	mode := ifidnModeMap[it.val]
	ret, err := mode.compareFn(p, it.params[0], it.params[1])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalIf(ret == mode.identical)
}

func ELSEIFDEF(p *parser, it *item) ErrorList {
	mode := it.val == "ELSEIFDEF"
	val, err := p.syms.Lookup(it.params[0])
	return err.AddL(p.evalElseif(it.val, (val != nil) == mode))
}

func ELSEIF(p *parser, it *item) ErrorList {
	mode := it.val == "ELSEIF"
	ret, err := p.syms.evalBool(it.pos, it.params[0])
	return err.AddL(p.evalElseif(it.val, ret == mode))
}

func ELSEIFB(p *parser, it *item) ErrorList {
	mode := it.val == "ELSEIFB"
	ret, err := p.isBlank(it.params[0])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalElseif(it.val, ret == mode)
}

func ELSEIFIDN(p *parser, it *item) ErrorList {
	mode := ifidnModeMap[it.val[4:]]
	ret, err := mode.compareFn(p, it.params[0], it.params[1])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalElseif(it.val, ret == mode.identical)
}

func ELSE(p *parser, it *item) ErrorList {
	return p.evalElseif("ELSE", true)
}

func ENDIF(p *parser, it *item) ErrorList {
	if p.ifNest == 0 {
		return ErrorListF(ESWarning, "found ENDIF without a matching condition")
	}
	if p.ifMatch == p.ifNest {
		p.ifMatch--
		p.ifElse = false
	}
	p.ifNest--
	return nil
}

func OPTION(p *parser, it *item) ErrorList {
	var options = map[string](map[string]func()){
		"CASEMAP": {
			"NONE":      func() { p.caseSensitive = true },
			"NOTPUBLIC": func() { p.caseSensitive = false },
			"ALL":       func() { p.caseSensitive = false },
		},
	}
	for _, param := range it.params {
		key, val := splitColon(param)
		key = strings.ToUpper(key)
		val = strings.ToUpper(val)
		if opt, keyOK := options[key]; keyOK {
			if fn, valOK := opt[val]; valOK {
				fn()
			} else {
				return ErrorListF(ESWarning,
					"illegal value for OPTION %s: %s", key, val,
				)
			}
		}
	}
	return nil
}

func MACRO(p *parser, it *item) ErrorList {
	if p.macro.nest == 0 {
		p.macro.name = it.sym
		p.macro.start = it.num
	}
	p.macro.nest++
	return nil
}

func ENDM(p *parser, it *item) ErrorList {
	var macro asmMacro
	var err ErrorList
	if p.macro.nest == 1 && p.macro.name != "" {
		macro, err = p.newMacro(it.num)
		if err.Severity() < ESError {
			err = err.AddL(p.syms.Set(p.macro.name, macro, false))
		}
		p.macro.name = ""
	}
	p.macro.nest--
	return err
}

// Placeholder for any non-MACRO block terminated with ENDM
func DummyMacro(p *parser, it *item) ErrorList {
	p.macro.nest++
	return nil
}

// cpuFlag defines the flags for the @CPU value.
type cpuFlag int

const (
	cpu8086 cpuFlag = 1 << 0
	cpu186          = 1 << 1
	cpu286          = 1 << 2
	cpu386          = 1 << 3
	cpu486          = 1 << 4
	cpu586          = 1 << 5
	cpu686          = 1 << 6
	cpuPriv         = 1 << 7
	cpu8087         = 1 << 8
	cpu287          = 1 << 10 // yes, there's a gap
	cpu387          = 1 << 11
	cpuX64          = 1 << 12 // eh, whatever
)

func (p *parser) setCPU(directive string) (err ErrorList) {
	f8086 := cpu8086 | cpu8087
	f186 := f8086 | cpu186
	f286 := f186 | cpu286 | cpu287
	f386 := f286 | cpu386 | cpu387
	f486 := f386 | cpu486
	f586 := f486 | cpu586
	f686 := f586 | cpu686
	fX64 := f686 | cpuX64
	// 8087, 287, and 387 keep previous non-FPU settings.
	fCPUMask := cpuFlag(^(cpu8087 | cpu287 | cpu387))

	cpuMap := map[string]cpuFlag{
		"8086": f8086, "186": f186, "286": f286, "386": f386,
		"486": f486, "586": f586, "686": f686, "X64": fX64,
	}
	fpuMap := map[string]cpuFlag{
		"8087": cpu8087,
		"287":  cpu8087 | cpu287,
		"387":  cpu8087 | cpu287 | cpu387,
	}

	cpu := cpuFlag(0)
	lastPos := len(directive) - 1
	if last := directive[lastPos]; last == 'C' || last == 'N' {
		directive = directive[:lastPos]
	} else if last == 'P' {
		cpu |= cpuPriv
		directive = directive[:lastPos]
	}
	if flag, ok := cpuMap[directive]; ok {
		cpu |= flag
	} else if flag, ok := fpuMap[directive]; ok {
		cpu |= (p.intSyms.CPU & fCPUMask) | flag
	}
	wordsize := uint8(2)
	if cpu&cpuX64 != 0 {
		wordsize = 8
	} else if cpu&cpu386 != 0 {
		wordsize = 4
	}
	p.intSyms.CPU = cpu
	p.intSyms.WordSize = wordsize
	return err
}

func CPU(p *parser, it *item) ErrorList {
	// No difference between P or . as the first character, so...
	return p.setCPU(it.val[1:])
}

func SEGMENT(p *parser, it *item) ErrorList {
	wordsize := uint8(0)
	var attributes = map[string]func(){
		"USE16": func() { wordsize = 2 },
		"USE32": func() { wordsize = 4 },
		"USE64": func() { wordsize = 8 },
	}
	seg, errList := p.GetSegment(it.sym, false)
	if errList.Severity() >= ESError {
		return errList
	}
	if len(it.params) > 0 {
		for stream := NewLexStreamAt(it.pos, it.params[0]); stream.peek() != eof; {
			param, err := stream.nextSegmentParam()
			errList = errList.AddL(err)
			if attrib, ok := attributes[strings.ToUpper(param)]; ok {
				attrib()
			}
		}
	}
	if wordsize > p.intSyms.WordSize {
		var str string
		switch wordsize {
		case 4:
			str = "32-bit segments require at least a .386 CPU setting"
		case 8:
			str = "64-bit segments require at least a .X64 CPU setting"
		}
		return errList.AddF(ESError, str)
	}
	if wordsize != 0 {
		seg.wordsize = wordsize
	}
	p.segs = append(p.segs, seg)
	return errList
}

func STACK(p *parser, it *item) (err ErrorList) {
	if p.intSyms.StackGroup == nil {
		return ErrorListF(ESError, "model must be specified first")
	}
	size := int64(0)
	if len(it.params) > 0 {
		newsize, errSize := p.syms.evalInt(it.pos, it.params[0])
		err = err.AddL(errSize)
		if errSize.Severity() >= ESError {
			return err
		} else if newsize.n < 0 {
			return err.AddF(ESError,
				"count must be positive or zero: %s", newsize,
			)
		}
		size = newsize.n
	} else {
		size = 0x400
	}
	seg, errSeg := p.GetSegment("STACK", *p.intSyms.StackGroup == "DGROUP")
	err = err.AddL(errSeg)
	if err.Severity() >= ESError {
		return err
	}

	size -= int64(seg.width())
	if size < 0 {
		return err
	}
	data, errDup := NewDUPOperator(asmInt{n: size}, asmString('\x00'))
	err = err.AddL(errDup)
	if errDup.Severity() >= ESError {
		return err
	}
	return err.AddL(seg.AddData(nil, data))
}

func ENDS(p *parser, it *item) (err ErrorList) {
	var curSeg *asmSegment
	var curStruc *asmStruc
	var prevStruc *asmStruc
	if len(p.segs) >= 1 {
		curSeg = p.segs[len(p.segs)-1].(*asmSegment)
	}
	if len(p.strucs) >= 1 {
		curStruc = p.strucs[len(p.strucs)-1].(*asmStruc)
	}
	if len(p.strucs) >= 2 {
		prevStruc = p.strucs[len(p.strucs)-2].(*asmStruc)
	}

	if curSeg != nil && p.syms.Equal(curSeg.name, it.sym) {
		if curStruc != nil {
			err = ErrorListOpen(p.strucs)
			p.strucs = nil
		}
		p.segs = p.segs[:len(p.segs)-1]
		return err
	} else if curStruc != nil {
		// See STRUC for an explanation of this stupidity
		expSym := ""
		if prevStruc == nil {
			expSym = curStruc.name
		}
		if p.syms.Equal(it.sym, expSym) {
			constant := p.syntax != "TASM"
			if prevStruc == nil {
				err = p.syms.Set(curStruc.name, *curStruc, constant)
			} else {
				ptr := &asmPtr{sym: &curStruc.name, unit: curStruc}
				err = prevStruc.members.Set(curStruc.name, *curStruc, constant)
				prevStruc.AddData(ptr, curStruc)
			}
			p.strucs = p.strucs[:len(p.strucs)-1]
			return err
		}
	}
	return ErrorListF(ESError, "unmatched ENDS: %s", it.sym)
}

func GROUP(p *parser, it *item) (err ErrorList) {
	group, err := p.GetGroup(it.sym)
	if err.Severity() >= ESError {
		return err
	}
	for _, seg := range it.params {
		seg, errSeg := p.GetSegment(seg, false)
		err = err.AddL(errSeg)
		if errSeg.Severity() < ESError {
			err = err.AddL(group.Add(seg))
		}
	}
	return err
}

func DATA(p *parser, it *item) (err ErrorList) {
	wordsize := map[string]SimpleData{
		"DB": 1, "DW": 2, "DD": 4, "DF": 6, "DP": 6, "DQ": 8, "DT": 10,
	}[it.val]
	return p.EmitData(it, wordsize)
}

func LABEL(p *parser, it *item) ErrorList {
	size, err := p.syms.evalInt(it.pos, it.params[0])
	if err.Severity() < ESError {
		err = err.AddL(p.EmitPointer(it.sym, SimpleData(size.n)))
	}
	return err
}

// eval evaluates the given item, updates the parse state accordingly, and
// returns whether to keep it in the parser's instruction list.
func (p *parser) eval(it *item) (keep bool, err ErrorList) {
	k, ok := Keywords[it.val]
	if !(k.Type&Conditional != 0 || (p.ifMatch >= p.ifNest)) {
		return false, err
	} else if k.Type&Macro == 0 && p.macro.nest != 0 {
		return true, err
	} else if !ok {
		// Dropping the error on unknown directives/symbols for now
		if insSym, errSym := p.syms.Get(it.val); errSym == nil {
			switch insSym.(type) {
			case asmMacro:
				return p.expandMacro(insSym.(asmMacro), it)
			case asmStruc:
				struc := insSym.(asmStruc)
				fn := func(p *parser, it *item) ErrorList {
					return p.EmitData(it, &struc)
				}
				k = Keyword{fn, Optional, Data | SingleParam, Range{1, 1}}
			}
		}
	}
	if k.Type&Data != 0 && len(p.segs) == 0 && len(p.strucs) == 0 {
		return true, ErrorListF(ESError,
			"code or data emission requires a segment: %s", it,
		)
	} else if len(p.strucs) >= 1 && k.Type&(NoStruct) != 0 {
		return true, ErrorListF(ESError,
			"%s not allowed inside structure definition", it.val,
		)
	} else if k.Func != nil {
		if err = it.checkSyntaxFor(k); err.Severity() < ESError {
			return k.Type&Evaluated == 0, err.AddL(k.Func(p, it))
		}
	}
	return true, err
}

func (p *parser) evalNew(it *item) (err ErrorList) {
	keep, err := p.eval(it)
	if keep {
		p.instructions = append(p.instructions, *it)
	}
	return err
}

func Parse(filename string, syntax string, includePaths []string) (*parser, ErrorList) {
	p := &parser{syntax: syntax}
	syms := *NewSymMap(&p.caseSensitive, &p.intSyms)
	p.syms = syms
	p.setCPU("8086")

	filenamesym := filepath.Base(filename)
	if i := strings.IndexByte(filenamesym, '.'); i != -1 {
		filenamesym = filenamesym[:i]
	}
	p.intSyms.FileName = asmExpression(strings.ToUpper(filenamesym))
	p.intSyms.FileName8 = asmString(fmt.Sprintf("%-8s", filenamesym)[:8])

	err := p.StepIntoFile(filename, includePaths)
	if err.Severity() >= ESFatal {
		return p, err
	}

	// Pass 1; any non-fatal errors are ignored
	p.pass2 = false
	for p.file != nil && err.Severity() < ESFatal {
		it, errLex := p.lexItem(&p.file.stream)
		if errLex.Severity() >= ESFatal {
			return p, errLex
		} else if it != nil {
			it.num = len(p.instructions)
			if errEval := p.evalNew(it); errEval.Severity() >= ESFatal {
				return p, err.AddLAt(it.pos, errEval)
			}
		} else {
			p.file = p.file.prev
		}
	}
	// Clear the state of nested blocks before starting the next pass.
	// Otherwise, we'd report all unclosed segments once per pass.
	p.segs = nil
	p.strucs = nil

	// Pass 2
	p.pass2 = true
	for i := range p.instructions {
		_, errEval := p.eval(&p.instructions[i])
		err = err.AddLAt(p.instructions[i].pos, errEval)
		if errEval.Severity() >= ESFatal {
			return p, err
		}
	}

	posEOF := NewItemPos(&filename, 0)
	err = err.AddLAt(posEOF, ErrorListOpen(p.strucs))
	err = err.AddLAt(posEOF, ErrorListOpen(p.segs))
	if p.proc.nest != 0 {
		err = err.AddFAt(posEOF, ESWarning,
			"ignoring procedure without an ENDP directive: %s", p.proc.name,
		)
	}
	return p, err
}
