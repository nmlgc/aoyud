// Assembly syntax parser.

package main

import (
	"fmt"
	"strconv"
	"strings"
)

type asmVal interface {
	Thing() string // Returns a singular noun describing this type of value
	width() uint   // Returns the width in bytes of the data value
	fmt.Stringer
}

type Nestable interface {
	asmVal
	// Returns "open <type of block>".
	OpenThing() string
	// Returns "open <types of block>".
	OpenThings() string
	// Returns a friendly name of the current block.
	Name() string
	// Returns a pointer to the block at the previous nesting level or an
	// explicit nil if no more nested blocks are left.
	Prev() Nestable
}

// asmInt represents an integer that will be output in a defined base.
type asmInt struct {
	n    int64  // The value itself
	ptr  uint64 // Nonzero values turn the integer into a pointer of this length
	base int
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
		ret = strconv.FormatInt(v.n, v.base)
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
	} else if v.base == 256 {
		ret = quoteASCII(v.formatASCII())
	}
	if v.ptr != 0 {
		ret = "(" + strconv.FormatUint(v.ptr, 10) + "*) " + ret
	}
	return ret
}

// isAsmInt checks whether input is to be interpreted as a single integer
// constant.
func isAsmInt(input string) bool {
	if len(input) == 0 {
		return false
	}
	f := input[0]
	if (f == '+' || f == '-') && len(input) == 1 {
		return false
	}
	validFirst := (f >= '0' && f <= '9')
	return validFirst && (strings.IndexAny(input, " \t") == -1)
}

// newAsmInt parses the input as an integer constant.
func newAsmInt(input string) (asmInt, *ErrorList) {
	length := len(input)
	base := 0
	switch input[length-1] {
	case 'b':
		base = 2
	case 'o':
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
	n, err := strconv.ParseInt(input, base, 0)
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

func (v asmExpression) width() uint {
	return uint(len(v))
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

func (v asmMacro) width() uint {
	return 0
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
func (p *parser) newMacro(itemNum int) (asmMacro, *ErrorList) {
	var err *ErrorList
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
				return asmMacro{}, ErrorListFAt(&header.pos, ESError,
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
				err = err.AddFAt(&header.pos, ESWarning,
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
				err = err.AddFAt(&code[i].pos, ESError,
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
// calls p.eval for every line in the macro. Returns false if the expansion
// was successful, true otherwise.
func (p *parser) expandMacro(m asmMacro, it *item) (bool, *ErrorList) {
	var errList *ErrorList
	replaceMap := make(map[string]string)

	setArg := func(name string, i int) (bool, *ErrorList) {
		var text string
		var err *ErrorList
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
		for stream := newLexStream(s); stream.peek() != eof; {
			// Be sure to copy any whitespace in s.
			start := stream.c
			stream.ignore(&whitespace)
			ret += s[start:stream.c]

			token := stream.nextToken(&shuntDelim)
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
			var err *ErrorList
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
		posCopy := make(ItemPos, len(it.pos), len(it.pos)+len(m.code[i].pos))
		copy(posCopy, it.pos)
		expanded := item{
			pos:    append(posCopy, m.code[i].pos...),
			typ:    m.code[i].typ,
			sym:    replace(&m.code[i], m.code[i].sym),
			val:    replace(&m.code[i], m.code[i].val),
			params: make([]string, len(m.code[i].params)),
		}
		for p := range m.code[i].params {
			expanded.params[p] = replace(&m.code[i], m.code[i].params[p])
		}
		errList = errList.AddLAt(&expanded.pos, p.eval(&expanded))
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
func ErrorListOpen(block Nestable) *ErrorList {
	str := block.OpenThing() + ": "
	if block.Prev() != nil {
		str = block.OpenThings() + ": "
	}
	str += block.Name()
	for block := block.Prev(); block != nil; block = block.Prev() {
		str += " ← " + block.Name()
	}
	return ErrorListF(ESWarning, str)
}

type parser struct {
	instructions []item
	// General state
	file            *parseFile
	syntax          string
	syms            SymMap
	macroLocalCount int // Number of LOCAL directives expanded
	// Open blocks
	proc    NestInfo
	macro   NestInfo
	struc   *asmStruc
	seg     *asmSegment
	segNest int // Tracks the number of open SEGMENT blocks
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

func (it *item) missingRequiredSym() *ErrorList {
	if it.sym == "" {
		return ErrorListF(ESError, "%s needs a name", it.val)
	}
	return nil
}

func (it *item) checkSyntaxFor(k Keyword) *ErrorList {
	if k.Sym == Mandatory {
		if err := it.missingRequiredSym(); err != nil {
			return err
		}
	}
	return it.checkParamRange(k.ParamRange)
}

func PROC(p *parser, it *item) *ErrorList {
	var err *ErrorList
	if p.proc.nest == 0 {
		p.proc.name = it.sym
		p.proc.start = len(p.instructions)
	} else {
		err = ErrorListF(ESWarning, "ignoring nested procedure %s", it.sym)
	}
	p.proc.nest++
	return err
}

func ENDP(p *parser, it *item) *ErrorList {
	var err *ErrorList
	if p.proc.nest == 0 {
		return ErrorListF(ESDebug,
			"ignoring procedure without a PROC directive: %s", it.sym,
		)
	} else if p.proc.nest == 1 {
		err = ErrorListF(ESDebug,
			"found procedure %s ranging from lex items #%d-#%d",
			p.proc.name, p.proc.start, len(p.instructions),
		)
	}
	p.proc.nest--
	return err
}

func MODEL(p *parser, it *item) *ErrorList {
	var err *ErrorList
	type modelVals struct {
		model, codesize, datasize int64
	}

	var modelValMap = map[string]modelVals{
		"TINY":    {1, 0, 0},
		"SMALL":   {2, 0, 0},
		"COMPACT": {3, 0, 1},
		"MEDIUM":  {4, 1, 0},
		"LARGE":   {5, 1, 1},
		"HUGE":    {6, 1, 2},
		"TCHUGE":  {7, 1, 2},
		"TPASCAL": {0, 0, 1},
		// Yes, the TASM manual actually got @Model wrong.
		// For MASM, @Model is changed to 7.
		"FLAT": {1, 0, 0},
	}

	// interfaceSym defines values for the @Interface symbol.
	var interfaceSym = map[string]asmInt{
		"NOLANGUAGE": {n: 0},
		"C":          {n: 1},
		"SYSCALL":    {n: 2},
		"STDCALL":    {n: 3},
		"PASCAL":     {n: 4},
		"FORTRAN":    {n: 5},
		"BASIC":      {n: 6},
		"FASTCALL":   {n: 7}, // MASM only
		"PROLOG":     {n: 7},
		"CPP":        {n: 8},
	}

	paramCount := len(it.params)
	model := strings.ToUpper(it.params[0])
	if m, ok := modelValMap[model]; ok {
		if model == "FLAT" {
			if p.syms.Map["@CPU"].Val.(asmInt).n&cpu386 == 0 {
				return err.AddF(ESError,
					"FLAT model requires at least a .386 CPU",
				)
			} else if p.syntax == "MASM" {
				m.model = 7
			}
		}
		err = err.AddL(p.syms.Set("@MODEL", asmInt{n: m.model}, false))
		err = err.AddL(p.syms.Set("@CODESIZE", asmInt{n: m.codesize}, false))
		err = err.AddL(p.syms.Set("@DATASIZE", asmInt{n: m.datasize}, false))
	} else {
		err = err.AddF(ESError, "invalid memory model: %s", model)
	}
	if paramCount > 1 {
		language := strings.ToUpper(it.params[1])
		if interfaceVal, ok := interfaceSym[language]; ok {
			err = err.AddL(p.syms.Set("@INTERFACE", interfaceVal, false))
		} else {
			err = err.AddF(ESError, "invalid language: %s", language)
		}
	} else {
		err = err.AddL(p.syms.Set("@INTERFACE", interfaceSym["NOLANGUAGE"], false))
	}
	return err
}

func EQUALS(p *parser, it *item) *ErrorList {
	ret, err := p.syms.evalInt(it.params[0])
	if err == nil {
		return p.syms.Set(it.sym, *ret, false)
	}
	return err
}

func EQU(p *parser, it *item) (err *ErrorList) {
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
		number, numberErr := p.syms.evalInt(it.params[0])
		if numberErr.Severity() < ESError {
			err = err.AddL(numberErr)
			return err.AddL(p.syms.Set(it.sym, *number, true))
		}
	}
	return p.syms.Set(it.sym, asmExpression(it.params[0]), false)
}

// text evaluates s as a text string used in a conditional directive.
func (p *parser) text(s string) (string, *ErrorList) {
	fail := func() (string, *ErrorList) {
		return "", ErrorListF(ESError,
			"invalid <text string> or %%text_macro: %s", s,
		)
	}
	if s[0] == '<' {
		var err *ErrorList
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

func (p *parser) isBlank(s string) (bool, *ErrorList) {
	ret, err := p.text(s)
	return len(ret) == 0, err
}

func (p *parser) isEqual(s1, s2 string) (bool, *ErrorList) {
	ret1, err1 := p.text(s1)
	ret2, err2 := p.text(s2)
	return ret1 == ret2, err1.AddL(err2)
}

func (p *parser) isEqualFold(s1, s2 string) (bool, *ErrorList) {
	ret1, err1 := p.text(s1)
	ret2, err2 := p.text(s2)
	return strings.EqualFold(ret1, ret2), err1.AddL(err2)
}

func (p *parser) evalIf(match bool) *ErrorList {
	valid := match && p.ifMatch == p.ifNest
	if valid {
		p.ifMatch++
	}
	p.ifNest++
	p.ifElse = !valid
	return nil
}

func (p *parser) evalElseif(directive string, match bool) *ErrorList {
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
	compareFn func(*parser, string, string) (bool, *ErrorList)
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

func IFDEF(p *parser, it *item) *ErrorList {
	mode := it.val == "IFDEF"
	val, err := p.syms.Lookup(it.params[0])
	return err.AddL(p.evalIf((val != nil) == mode))
}

func IF(p *parser, it *item) *ErrorList {
	mode := it.val == "IF"
	ret, err := p.syms.evalBool(it.params[0])
	return err.AddL(p.evalIf(ret == mode))
}

func IFB(p *parser, it *item) *ErrorList {
	mode := it.val == "IFB"
	ret, err := p.isBlank(it.params[0])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalIf(ret == mode)
}

func IFIDN(p *parser, it *item) *ErrorList {
	mode := ifidnModeMap[it.val]
	ret, err := mode.compareFn(p, it.params[0], it.params[1])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalIf(ret == mode.identical)
}

func ELSEIFDEF(p *parser, it *item) *ErrorList {
	mode := it.val == "ELSEIFDEF"
	val, err := p.syms.Lookup(it.params[0])
	return err.AddL(p.evalElseif(it.val, (val != nil) == mode))
}

func ELSEIF(p *parser, it *item) *ErrorList {
	mode := it.val == "ELSEIF"
	ret, err := p.syms.evalBool(it.params[0])
	return err.AddL(p.evalElseif(it.val, ret == mode))
}

func ELSEIFB(p *parser, it *item) *ErrorList {
	mode := it.val == "ELSEIFB"
	ret, err := p.isBlank(it.params[0])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalElseif(it.val, ret == mode)
}

func ELSEIFIDN(p *parser, it *item) *ErrorList {
	mode := ifidnModeMap[it.val[4:]]
	ret, err := mode.compareFn(p, it.params[0], it.params[1])
	if err.Severity() >= ESError {
		return err
	}
	return p.evalElseif(it.val, ret == mode.identical)
}

func ELSE(p *parser, it *item) *ErrorList {
	return p.evalElseif("ELSE", true)
}

func ENDIF(p *parser, it *item) *ErrorList {
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

func OPTION(p *parser, it *item) *ErrorList {
	var options = map[string](map[string]func()){
		"CASEMAP": {
			"NONE":      func() { p.syms.CaseSensitive = true },
			"NOTPUBLIC": func() { p.syms.CaseSensitive = false },
			"ALL":       func() { p.syms.CaseSensitive = false },
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

func MACRO(p *parser, it *item) *ErrorList {
	if p.macro.nest == 0 {
		p.macro.name = it.sym
		p.macro.start = len(p.instructions)
	}
	p.macro.nest++
	return nil
}

func ENDM(p *parser, it *item) *ErrorList {
	var macro asmMacro
	var err *ErrorList
	if p.macro.nest == 1 && p.macro.name != "" {
		macro, err = p.newMacro(len(p.instructions))
		if err.Severity() < ESError {
			err = err.AddL(p.syms.Set(p.macro.name, macro, false))
		}
		p.macro.name = ""
	}
	p.macro.nest--
	return err
}

// Placeholder for any non-MACRO block terminated with ENDM
func DummyMacro(p *parser, it *item) *ErrorList {
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

func (p *parser) setCPU(directive string) *ErrorList {
	var err *ErrorList
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
		if prevCPU, ok := p.syms.Map["@CPU"]; ok {
			cpu |= cpuFlag(prevCPU.Val.(asmInt).n) & fCPUMask
		}
		cpu |= flag
	}
	wordsize := int64(2)
	if cpu&cpuX64 != 0 {
		wordsize = 8
	} else if cpu&cpu386 != 0 {
		wordsize = 4
	}
	err = err.AddL(p.syms.Set("@CPU", asmInt{n: int64(cpu), base: 2}, false))
	err = err.AddL(p.syms.Set("@WORDSIZE", asmInt{n: wordsize}, false))
	return err
}

func CPU(p *parser, it *item) *ErrorList {
	// No difference between P or . as the first character, so...
	return p.setCPU(it.val[1:])
}

func SEGMENT(p *parser, it *item) *ErrorList {
	cpuWordSize := uint(p.syms.Map["@WORDSIZE"].Val.(asmInt).n) // can never fail
	seg := &asmSegment{}
	var old asmVal
	var errList *ErrorList
	var attributes = map[string]func(){
		"USE16": func() { seg.wordsize = 2 },
		"USE32": func() { seg.wordsize = 4 },
		"USE64": func() { seg.wordsize = 8 },
	}
	if old, errList = p.syms.Lookup(it.sym); old != nil {
		switch old.(type) {
		case *asmSegment:
			seg = old.(*asmSegment)
		default:
			// SymMap.Set handles this error message. Just continuing to
			// create the segment as normal certainly leads to fewer errors.
		}
	} else {
		seg.wordsize = cpuWordSize
		seg.name = it.sym
	}
	if len(it.params) > 0 {
		for stream := newLexStream(it.params[0]); stream.peek() != eof; {
			param, err := stream.nextSegmentParam()
			errList = errList.AddL(err)
			if attrib, ok := attributes[strings.ToUpper(param)]; ok {
				attrib()
			}
		}
	}
	if seg.wordsize > cpuWordSize {
		var str string
		switch seg.wordsize {
		case 4:
			str = "32-bit segments require at least a .386 CPU setting"
		case 8:
			str = "64-bit segments require at least a .X64 CPU setting"
		}
		return errList.AddF(ESError, str)
	}
	seg.prev = p.seg
	p.seg = seg
	p.segNest++
	return errList.AddL(p.syms.Set(it.sym, seg, false))
}

func ENDS(p *parser, it *item) *ErrorList {
	if p.seg != nil && p.syms.Equal(p.seg.name, it.sym) {
		var err *ErrorList
		if p.struc != nil {
			err = ErrorListOpen(p.struc)
			p.struc = nil
		}
		p.seg = p.seg.prev
		p.segNest--
		return err
	} else if p.struc != nil {
		// See parseSTRUC for an explanation of this stupidity
		expSym := ""
		if p.struc.prev == nil {
			expSym = p.struc.name
		}
		if p.syms.Equal(it.sym, expSym) {
			p.struc = p.struc.prev
			return nil
		}
	}
	return ErrorListF(ESError, "unmatched ENDS: %s", it.sym)
}

func DATA(p *parser, it *item) *ErrorList {
	var widthMap = map[string]uint{
		"DB": 1, "DW": 2, "DD": 4, "DF": 6, "DP": 6, "DQ": 8, "DT": 10,
	}
	if it.sym != "" && p.seg != nil {
		ptr := asmDataPtr{seg: p.seg, off: -1, w: widthMap[it.val]}
		return p.syms.Set(it.sym, ptr, true)
	}
	return nil
}

func LABEL(p *parser, it *item) *ErrorList {
	size, err := p.syms.evalInt(it.params[0])
	if size != nil && p.seg != nil {
		ptr := asmDataPtr{seg: p.seg, off: -1, w: uint(size.n)}
		return err.AddL(p.syms.Set(it.sym, ptr, true))
	}
	return err
}

// eval evaluates the given item, updates the parse state accordingly, and
// keeps it in the parser's instruction list if necessary.
func (p *parser) eval(it *item) (err *ErrorList) {
	var typ KeywordType = 0
	k, ok := Keywords[it.val]
	if ok {
		typ = k.Type
	}
	if !(typ&Conditional != 0 || (p.ifMatch >= p.ifNest)) {
		return
	}
	ret := true
	if typ&Macro != 0 || p.macro.nest == 0 {
		if ok {
			if typ&Emit != 0 && p.seg == nil && p.struc == nil {
				err = ErrorListF(ESError,
					"code or data emission requires a segment: %s", it,
				)
			} else if p.struc != nil && typ&(CodeBlock|EmitCode) != 0 {
				err = ErrorListF(ESError,
					"%s not allowed inside structure definition", it.val,
				)
			} else if k.Func != nil {
				if err = it.checkSyntaxFor(k); err.Severity() < ESError {
					err = k.Func(p, it)
					ret = typ&Evaluated == 0
				}
			}
		} else // Dropping the error on unknown directives/symbols for now
		if insSym, errSym := p.syms.Get(it.val); errSym == nil {
			switch insSym.(type) {
			case asmMacro:
				ret, err = p.expandMacro(insSym.(asmMacro), it)
			}
		}
	}
	if ret {
		p.instructions = append(p.instructions, *it)
	}
	return err
}

func Parse(filename string, syntax string, includePaths []string) (*parser, *ErrorList) {
	p := &parser{syntax: syntax, syms: *NewSymMap()}
	p.setCPU("8086")

	err := p.StepIntoFile(filename, includePaths)

	for p.file != nil && err.Severity() < ESFatal {
		it, lexErr := p.lexItem()
		err = err.AddL(lexErr)
		if it != nil && lexErr.Severity() < ESFatal {
			evalErr := p.eval(it)
			err = err.AddLAt(&it.pos, evalErr)
		}
	}

	posEOF := NewItemPos(&filename, 0)
	if p.struc != nil {
		err = err.AddLAt(posEOF, ErrorListOpen(p.struc))
	}
	if p.segNest != 0 {
		err = err.AddLAt(posEOF, ErrorListOpen(p.seg))
	}
	if p.proc.nest != 0 {
		err = err.AddFAt(posEOF, ESWarning,
			"ignoring procedure without an ENDP directive: %s", p.proc.name,
		)
	}
	return p, err
}
