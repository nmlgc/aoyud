// Assembly syntax parser.

package main

import (
	"fmt"
	"log"
	"sort"
	"strconv"
	"strings"
)

type asmVal interface {
	width() uint // Returns the width in bytes of the data value
	fmt.Stringer
}

// asmInt represents an integer that will be output in a defined base.
type asmInt struct {
	n    int64  // The value itself
	ptr  uint64 // Nonzero values turn the integer into a pointer of this length
	base int
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
	if v.base == 0 {
		v.base = 10
	}
	ret := strconv.FormatInt(v.n, v.base)
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
func newAsmInt(input string) (asmInt, error) {
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
		return asmInt{}, err
	}
	return asmInt{n: n, base: base}, nil
}

type asmString string

func (v asmString) width() uint {
	return uint(len(v))
}

func (v asmString) String() string {
	return "\"" + string(v) + "\""
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
	locals []string
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
		ret += "\n\tLOCAL\t" + strings.Join(v.locals, ", ")
	}
	ret += "\n"
	for _, ins := range v.code {
		ret += ins.String()
	}
	return ret + "\tENDM"
}

// newMacro creates a new multiline macro ending at itemNum.
func (p *parser) newMacro(itemNum int) (asmMacro, error) {
	header := p.instructions[p.macro.start]
	args := make([]asmMacroArg, len(header.params))
	for i := range header.params {
		nameOrg, typOrg := splitColon(header.params[i])
		args[i].name = p.toSymCase(nameOrg)
		args[i].typ = strings.ToUpper(typOrg)
		// Verify types
		switch args[i].typ {
		case "":
		case "REQ":
			break
		case "REST":
		case "VARARG":
			if i != len(header.params)-1 {
				// TASM would actually accept this, but we better
				// complain since it doesn't make sense at all.
				return asmMacro{}, fmt.Errorf(
					"macro %s: %s:%s must be the last parameter\n",
					p.macro.name, args[i].name, args[i].typ,
				)
			}
		default:
			if typOrg[0] == '=' {
				def, err := p.text(strings.TrimSpace(typOrg[1:]))
				if err != nil {
					return asmMacro{}, err
				}
				args[i].typ = "="
				args[i].def = def
			} else {
				return asmMacro{}, fmt.Errorf(
					"macro %s: invalid argument type: %s",
					p.macro.name, args[i].typ,
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
					locals = append(locals, p.toSymCase(param))
				}
				code = code[i+1:]
				i--
			} else {
				log.Printf(
					"LOCAL directives must come first in a macro body, ignoring: %s",
					strings.Join(code[i].params, ", "),
				)
			}
		} else {
			localsAllowed = false
		}
	}
	return asmMacro{args, code, locals}, nil
}

// expandMacro expands the multiline macro m using the given params and calls
// p.eval for every line in the macro. Returns false if the expansion was
// successful, true otherwise.
func (p *parser) expandMacro(m asmMacro, params []string) bool {
	replaceMap := make(map[string]string)

	setArg := func(name string, i int) bool {
		ret := len(params) > i && len(params[i]) > 0
		if ret {
			if params[i][0] == '<' || params[i][0] == '%' {
				text, err := p.text(params[i])
				if err != nil {
					log.Println(err)
					return false
				}
				replaceMap[name] = text
			} else {
				replaceMap[name] = params[i]
			}
		}
		return ret
	}

	replace := func(s string) string {
		ret := ""
		andCached := false
		for stream := newLexStream(s); stream.peek() != eof; {
			// Be sure to copy any whitespace in s.
			start := stream.pos
			stream.ignore(&whitespace)
			ret += s[start:stream.pos]

			token := stream.nextToken(&shuntDelim)
			if token == "&" {
				andCached = true
				token = ""
			} else if arg, ok := replaceMap[p.toSymCase(token)]; ok {
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
		replaceMap[arg.name] = arg.def
		switch arg.typ {
		case "REST":
		case "VARARG":
			replaceMap[arg.name] = strings.Join(params[i:], ", ")
		case "REQ":
			if !setArg(arg.name, i) {
				log.Printf("macro argument #%d (%s) is required", i, arg.name)
				return true
			}
		default:
			setArg(arg.name, i)
		}
	}
	for _, local := range m.locals {
		// Who knows, some code might actually rely on the resulting
		// labels being named exactly like this.
		replaceMap[local] = fmt.Sprintf("??%04X", p.macroLocalCount)
		p.macroLocalCount++
	}
	for i := range m.code {
		expanded := item{
			typ:    m.code[i].typ,
			sym:    replace(m.code[i].sym),
			val:    replace(m.code[i].val),
			params: make([]string, len(m.code[i].params)),
		}
		for p := range m.code[i].params {
			expanded.params[p] = replace(m.code[i].params[p])
		}
		p.eval(&expanded)
	}
	return false
}

type symbol struct {
	constant bool
	val      asmVal
}

func (s symbol) String() string {
	var ret string
	if s.constant {
		ret = "(const) "
	}
	return ret + s.val.String() + "\n"
}

type symMap map[string]symbol

// nestableBlock represents a type of named block that can be nested.
type nestableBlock struct {
	name  string // Name of level 1
	start int    // First item in the instruction list that belongs to level 1
	nest  int    // Current nesting level
}

type parser struct {
	instructions []item
	// General state
	syntax          string
	syms            symMap
	symCase         bool // case sensitivity for symbols
	macroLocalCount int  // Number of LOCAL directives expanded
	// Open blocks
	proc  nestableBlock
	macro nestableBlock
	// Conditionals
	ifNest  int  // IF nesting level
	ifMatch int  // Last IF nesting level that evaluated to true
	ifElse  bool // Can the current level still have an ELSE* block?
}

func (p *parser) toSymCase(s string) string {
	if !p.symCase {
		return strings.ToUpper(s)
	}
	return s
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

type parseFnType int

const (
	typeConditional parseFnType = (1 << iota) // Not kept in the parser's instruction list
	typeMacro                   = (1 << iota)
)

// parseFn represents a function handling a certain instruction or directive
// at parsing time. The return value indicates whether the instruction should
// stay in the parser's instruction list.
type parseFn struct {
	f          func(p *parser, itemNum int, i *item) bool
	typ        parseFnType
	paramRange Range
}

func (p *parser) parsePROC(itemNum int, i *item) bool {
	if p.proc.nest == 0 {
		p.proc.name = i.sym
		p.proc.start = itemNum
	} else {
		log.Printf("ignoring nested procedure %s\n", i.sym)
	}
	p.proc.nest++
	return true
}

func (p *parser) parseENDP(itemNum int, i *item) bool {
	if p.proc.nest == 0 {
		log.Printf("ignoring procedure %s without a PROC directive\n", i.sym)
	} else if p.proc.nest == 1 {
		log.Printf(
			"found procedure %s ranging from lex items #%d-#%d\n",
			p.proc.name, p.proc.start, itemNum,
		)
	}
	p.proc.nest--
	return true
}

func (p *parser) parseMODEL(itemNum int, i *item) bool {
	// modelSym defines the @Model value for each memory model.
	var modelSym = map[string]asmInt{
		"TINY":  {n: 1},
		"SMALL": {n: 2},
		// Yes, the TASM manual is actually wrong here.
		// For MASM, this is changed to 7.
		"FLAT":    {n: 1},
		"COMPACT": {n: 3},
		"MEDIUM":  {n: 4},
		"LARGE":   {n: 5},
		"HUGE":    {n: 6},
		"TCHUGE":  {n: 7},
		"TPASCAL": {n: 0},
	}

	// modelCodeSize defines the @CodeSize value for each memory model.
	var modelCodeSize = map[string]asmInt{
		"TINY":    {n: 0},
		"SMALL":   {n: 0},
		"COMPACT": {n: 0},
		"MEDIUM":  {n: 1},
		"LARGE":   {n: 1},
		"HUGE":    {n: 1},
		"TCHUGE":  {n: 1},
		"TPASCAL": {n: 0},
		"FLAT":    {n: 0},
	}

	// modelDataSize defines the @DataSize value for each memory model.
	var modelDataSize = map[string]asmInt{
		"TINY":    {n: 0},
		"SMALL":   {n: 0},
		"COMPACT": {n: 1},
		"MEDIUM":  {n: 0},
		"LARGE":   {n: 1},
		"HUGE":    {n: 2},
		"TCHUGE":  {n: 2},
		"TPASCAL": {n: 1},
		"FLAT":    {n: 0},
	}

	// interfaceSym defines values for the @Interface symbol
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

	paramCount := len(i.params)
	model := strings.ToUpper(i.params[0])
	if modelVal, ok := modelSym[model]; ok {
		if p.syntax == "MASM" && model == "FLAT" {
			modelVal.n = 7
		}
		p.setSym("@MODEL", modelVal, false)
		p.setSym("@CODESIZE", modelCodeSize[model], false)
		p.setSym("@DATASIZE", modelDataSize[model], false)
	} else {
		log.Printf("invalid memory model: %s\n", model)
	}
	if paramCount > 1 {
		language := strings.ToUpper(i.params[1])
		if interfaceVal, ok := interfaceSym[language]; ok {
			p.setSym("@INTERFACE", interfaceVal, false)
		} else {
			log.Printf("invalid language: %s\n", language)
		}
	} else {
		p.setSym("@INTERFACE", interfaceSym["NOLANGUAGE"], false)
	}
	return true
}

func (p *parser) parseEQUALS(itemNum int, i *item) bool {
	if rpnStack := p.shunt(i.params[0]); rpnStack != nil {
		if ret := rpnStack.solve(); ret != nil {
			p.setSym(i.sym, *ret, false)
		}
	}
	return true
}

func (p *parser) parseEQU(itemNum int, i *item) bool {
	p.setSym(i.sym, asmString(i.params[0]), true)
	return true
}

// text evaluates s as a text string used in a conditional directive.
func (p *parser) text(s string) (string, error) {
	fail := func() (string, error) {
		return "", fmt.Errorf("invalid <text string> or %text_macro: %s", s)
	}
	if s[0] == '<' {
		s = s[1:]
		// TASM does not strip whitespace here, JWasm does.
		if p.syntax == "MASM" {
			s = strings.TrimSpace(s)
		}
		rb := strings.IndexByte(s, '>')
		if rb == -1 {
			return fail()
		} else if rb != len(s)-1 {
			log.Printf("extra characters on line")
		}
		return s[:rb], nil
	} else if s[0] == '%' {
		name := strings.TrimSpace(p.toSymCase(s[1:]))
		sym, err := p.getSym(name)
		if err != nil {
			return "", err
		}
		switch sym.(type) {
		case asmInt:
			return strconv.FormatInt(sym.(asmInt).n, 10), nil
		case asmString:
			return string(sym.(asmString)), nil
		case asmMacro:
			return "", fmt.Errorf("can't use macro name in expression: %s", name)
		default:
			return "", fmt.Errorf("invalid symbol type in expression: %s", sym)
		}
	}
	return fail()
}

func (p *parser) isBlank(s string) (bool, error) {
	ret, err := p.text(s)
	return len(ret) == 0, err
}

func (p *parser) isEqual(s1, s2 string) (bool, error) {
	ret1, err1 := p.text(s1)
	ret2, err2 := p.text(s2)
	if err1 != nil {
		log.Println(err1)
	}
	return ret1 == ret2, err2
}

func (p *parser) isEqualFold(s1, s2 string) (bool, error) {
	ret1, err1 := p.text(s1)
	ret2, err2 := p.text(s2)
	if err1 != nil {
		log.Println(err1)
	}
	return strings.EqualFold(ret1, ret2), err2
}

func (p *parser) evalIf(match bool) bool {
	valid := match && p.ifMatch == p.ifNest
	if valid {
		p.ifMatch++
	}
	p.ifNest++
	p.ifElse = !valid
	return false
}

func (p *parser) evalElseif(directive string, match bool) bool {
	if p.ifNest == 0 {
		log.Println("unmatched", directive)
		return true
	}
	if p.ifMatch == p.ifNest {
		p.ifMatch--
	} else if p.ifMatch == (p.ifNest-1) && p.ifElse && match {
		p.ifMatch++
		p.ifElse = false
	}
	return false
}

type ifidnMode struct {
	compareFn func(*parser, string, string) (bool, error)
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

func (p *parser) parseIFDEF(itemNum int, i *item) bool {
	_, defined := p.syms[p.toSymCase(i.params[0])]
	mode := i.val == "IFDEF"
	return p.evalIf(defined == mode)
}

func (p *parser) parseIF(itemNum int, i *item) bool {
	mode := i.val == "IF"
	return p.evalIf(p.evalBool(i.params[0]) == mode)
}

func (p *parser) parseIFB(itemNum int, i *item) bool {
	mode := i.val == "IFB"
	ret, err := p.isBlank(i.params[0])
	if err != nil {
		log.Println(err)
		return true
	}
	return p.evalIf(ret == mode)
}

func (p *parser) parseIFIDN(itemNum int, i *item) bool {
	mode := ifidnModeMap[i.val]
	ret, err := mode.compareFn(p, i.params[0], i.params[1])
	if err != nil {
		log.Println(err)
		return true
	}
	return p.evalIf(ret == mode.identical)
}

func (p *parser) parseELSEIFDEF(itemNum int, i *item) bool {
	_, defined := p.syms[p.toSymCase(i.params[0])]
	mode := i.val == "ELSEIFDEF"
	return p.evalElseif(i.val, defined == mode)
}

func (p *parser) parseELSEIF(itemNum int, i *item) bool {
	mode := i.val == "ELSEIF"
	return p.evalElseif(i.val, p.evalBool(i.params[0]) == mode)
}

func (p *parser) parseELSEIFB(itemNum int, i *item) bool {
	ret, err := p.isBlank(i.params[0])
	if err != nil {
		log.Println(err)
		return true
	}
	mode := i.val == "ELSEIFB"
	return p.evalElseif(i.val, ret == mode)
}

func (p *parser) parseELSEIFIDN(itemNum int, i *item) bool {
	mode := ifidnModeMap[i.val[4:]]
	ret, err := mode.compareFn(p, i.params[0], i.params[1])
	if err != nil {
		log.Println(err)
		return true
	}
	return p.evalElseif(i.val, ret == mode.identical)
}

func (p *parser) parseELSE(itemNum int, i *item) bool {
	return p.evalElseif("ELSE", true)
}

func (p *parser) parseENDIF(itemNum int, i *item) bool {
	if p.ifNest == 0 {
		log.Println("found ENDIF without a matching condition")
		return true
	}
	if p.ifMatch == p.ifNest {
		p.ifMatch--
		p.ifElse = false
	}
	p.ifNest--
	return false
}

func (p *parser) parseOPTION(itemNum int, i *item) bool {
	var options = map[string](map[string]func()){
		"CASEMAP": {
			"NONE":      func() { p.symCase = true },
			"NOTPUBLIC": func() { p.symCase = false },
			"ALL":       func() { p.symCase = false },
		},
	}
	for _, param := range i.params {
		key, val := splitColon(param)
		key = strings.ToUpper(key)
		val = strings.ToUpper(val)
		if opt, keyOK := options[key]; keyOK {
			if fn, valOK := opt[val]; valOK {
				fn()
			} else {
				log.Printf("illegal value for OPTION %s: %s", key, val)
			}
		}
	}
	return true
}

func (p *parser) parseMACRO(itemNum int, i *item) bool {
	if p.macro.nest == 0 {
		p.macro.name = i.sym
		p.macro.start = itemNum
	}
	p.macro.nest++
	return true
}

func (p *parser) parseENDM(itemNum int, i *item) bool {
	if p.macro.nest == 1 && p.macro.name != "" {
		macro, err := p.newMacro(itemNum)
		if err != nil {
			log.Println(err)
		} else {
			p.setSym(p.macro.name, macro, false)
		}
		p.macro.name = ""
	}
	p.macro.nest--
	return true
}

// Placeholder for any non-MACRO block terminated with ENDM
func (p *parser) parseDummyMacro(itemNum int, i *item) bool {
	p.macro.nest++
	return true
}

var parseFns = map[string]parseFn{
	"PROC":       {(*parser).parsePROC, 0, Range{0, -1}},
	"ENDP":       {(*parser).parseENDP, 0, pReq(0)},
	".MODEL":     {(*parser).parseMODEL, 0, Range{1, 6}},
	"=":          {(*parser).parseEQUALS, 0, pReq(1)},
	"EQU":        {(*parser).parseEQU, 0, Range{1, -1}},
	"IFDEF":      {(*parser).parseIFDEF, typeConditional, pReq(1)},
	"IFNDEF":     {(*parser).parseIFDEF, typeConditional, pReq(1)},
	"IF":         {(*parser).parseIF, typeConditional, pReq(1)},
	"IFE":        {(*parser).parseIF, typeConditional, pReq(1)},
	"IFB":        {(*parser).parseIFB, typeConditional, pReq(1)},
	"IFNB":       {(*parser).parseIFB, typeConditional, pReq(1)},
	"IFIDN":      {(*parser).parseIFIDN, typeConditional, pReq(2)},
	"IFIDNI":     {(*parser).parseIFIDN, typeConditional, pReq(2)},
	"IFDIF":      {(*parser).parseIFIDN, typeConditional, pReq(2)},
	"IFDIFI":     {(*parser).parseIFIDN, typeConditional, pReq(2)},
	"ELSEIFDEF":  {(*parser).parseELSEIFDEF, typeConditional, pReq(1)},
	"ELSEIFNDEF": {(*parser).parseELSEIFDEF, typeConditional, pReq(1)},
	"ELSEIF":     {(*parser).parseELSEIF, typeConditional, pReq(1)},
	"ELSEIFE":    {(*parser).parseELSEIF, typeConditional, pReq(1)},
	"ELSEIFB":    {(*parser).parseELSEIFB, typeConditional, pReq(1)},
	"ELSEIFNB":   {(*parser).parseELSEIFB, typeConditional, pReq(1)},
	"ELSEIFIDN":  {(*parser).parseELSEIFIDN, typeConditional, pReq(2)},
	"ELSEIFIDNI": {(*parser).parseELSEIFIDN, typeConditional, pReq(2)},
	"ELSEIFDIF":  {(*parser).parseELSEIFIDN, typeConditional, pReq(2)},
	"ELSEIFDIFI": {(*parser).parseELSEIFIDN, typeConditional, pReq(2)},
	"ELSE":       {(*parser).parseELSE, typeConditional, pReq(0)},
	"ENDIF":      {(*parser).parseENDIF, typeConditional, pReq(0)},
	"OPTION":     {(*parser).parseOPTION, 0, Range{1, -1}},
	// Macros
	"MACRO":  {(*parser).parseMACRO, typeMacro, Range{0, -1}},
	"FOR":    {(*parser).parseDummyMacro, typeMacro, pReq(2)},
	"FORC":   {(*parser).parseDummyMacro, typeMacro, Range{1, -1}}, // see JWasm's FORC.ASM
	"REPT":   {(*parser).parseDummyMacro, typeMacro, pReq(1)},
	"REPEAT": {(*parser).parseDummyMacro, typeMacro, pReq(1)},
	"WHILE":  {(*parser).parseDummyMacro, typeMacro, pReq(1)},
	"IRP":    {(*parser).parseDummyMacro, typeMacro, pReq(2)},
	"IRPC":   {(*parser).parseDummyMacro, typeMacro, pReq(2)},
	"ENDM":   {(*parser).parseENDM, typeMacro, pReq(0)},
}

// getSym returns the value of a symbol that is meant to exist in the map, or
// an error if it doesn't.
func (p *parser) getSym(name string) (asmVal, error) {
	if ret, ok := p.syms[p.toSymCase(name)]; ok {
		return ret.val, nil
	}
	return nil, fmt.Errorf("unknown symbol %s", name)
}

func (p *parser) setSym(name string, val asmVal, constant bool) bool {
	// TODO: Enforce constness for EQU while making sure that the cases in
	// JWasm's EQUATE6.ASM still work.
	realName := p.toSymCase(name)
	if existing := p.syms[realName]; existing.constant {
		log.Printf("constant symbol %s already defined elsewhere", realName)
		return false
	}
	p.syms[realName] = symbol{val: val, constant: constant}
	return true
}

// eval evaluates the given item, updates the parse state accordingly, and
// keeps i in the parser's instruction list, unless it lies on an ignored
// conditional branch.
func (p *parser) eval(i *item) {
	if p.syms == nil {
		p.syms = make(symMap)
	}
	var typ parseFnType = 0
	insUpper := strings.ToUpper(i.val)
	fn, ok := parseFns[insUpper]
	if ok {
		i.val = insUpper
		typ = fn.typ
	}
	if !(typ&typeConditional != 0 || (p.ifMatch >= p.ifNest)) {
		return
	}
	ret := true
	if i.typ == itemInstruction && (typ&typeMacro != 0 || (p.macro.nest == 0)) {
		if ok {
			if i.checkParamRange(fn.paramRange) {
				ret = fn.f(p, len(p.instructions), i)
			}
		} else if insSym, ok := p.getSym(i.val); ok == nil {
			switch insSym.(type) {
			case asmMacro:
				ret = p.expandMacro(insSym.(asmMacro), i.params)
			}
		}
	}
	if ret {
		p.instructions = append(p.instructions, *i)
	}
}

func (p *parser) end() {
	if p.proc.nest != 0 {
		log.Printf("ignoring procedure %s without an ENDP directive\n", p.proc.name)
	}
	if len(p.syms) > 0 {
		var keys []string
		for i := range p.syms {
			keys = append(keys, i)
		}
		sort.Strings(keys)
		log.Println("Symbols: [")
		defer log.SetPrefix(log.Prefix())
		log.SetPrefix("")
		for _, k := range keys {
			log.Printf("• %s: %s", k, p.syms[k])
		}
		log.Println("]")
	}
}
