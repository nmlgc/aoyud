// Assembly syntax parser.

package main

import (
	"fmt"
	"log"
	"sort"
	"strconv"
	"strings"
)

type asmVal fmt.Stringer

// asmInt represents an integer that will be output in a defined base.
type asmInt struct {
	n    int64
	base int
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
	return ret
}

// isAsmInt checks whether input is to be interpreted as a single integer
// constant.
func isAsmInt(input string) bool {
	if len(input) == 0 {
		return false
	}
	f := input[0]
	validFirst := ((f >= '0' && f <= '9') || f == '+' || f == '-')
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
			ret += v.def
		}
	}
	return ret
}

type asmMacro struct {
	args []asmMacroArg
	code []item
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
	ret += "\n"
	for _, ins := range v.code {
		ret += ins.String()
	}
	return ret + "\tENDM"
}

// newMacro creates a new multiline macro ending at itemNum.
func (p *parser) newMacro(itemNum int) (asmMacro, error) {
	start := p.instructions[p.macro.start]
	args := make([]asmMacroArg, len(start.params))
	for i := range start.params {
		nameOrg, typOrg := splitColon(start.params[i])
		args[i].name = p.toSymCase(nameOrg)
		args[i].typ = strings.ToUpper(typOrg)
		// Verify types
		switch args[i].typ {
		case "":
		case "REQ":
			break
		case "REST":
		case "VARARG":
			if i != len(start.params)-1 {
				// TASM would actually accept this, but we better
				// complain since it doesn't make sense at all.
				return asmMacro{}, fmt.Errorf(
					"macro %s: %s:%s must be the last parameter\n",
					p.macro.name, args[i].name, args[i].typ,
				)
			}
		default:
			if typOrg[0] == '=' {
				args[i].typ = "="
				args[i].def = strings.TrimSpace(typOrg[1:])
			} else {
				return asmMacro{}, fmt.Errorf(
					"macro %s: invalid argument type: %s",
					p.macro.name, args[i].typ,
				)
			}
		}
	}
	return asmMacro{
		code: p.instructions[p.macro.start+1 : itemNum],
		args: args,
	}, nil
}

// newAsmVal returns the correct type of assembly value for input.
func (p *parser) newAsmVal(input string) asmVal {
	if newval, err := p.shunt(input); err == nil {
		return newval
	} else {
		log.Println(err)
		return asmString(input)
	}
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
	syntax  string
	syms    symMap
	symCase bool   // case sensitivity for symbols
	symLast string // last symbol declaration encountered
	// Open blocks
	proc  nestableBlock
	macro nestableBlock
	// Conditionals
	ifNest  int // IF nesting level
	ifMatch int // Last IF nesting level that evaluated to true
	ifElse  int // Last IF nesting level that may have an ELSE* block
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

// parseFn represents a function handling a certain instruction or directive
// at parsing time. The return value indicates whether the instruction should
// stay in the parser's instruction list.
type parseFn struct {
	f         func(p *parser, itemNum int, i *item) bool
	minParams int
}

func (p *parser) parsePROC(itemNum int, i *item) bool {
	if p.proc.nest == 0 {
		p.proc.name = p.symLast
		p.proc.start = itemNum
	} else {
		log.Printf("ignoring nested procedure %s\n", p.symLast)
	}
	p.proc.nest++
	return true
}

func (p *parser) parseENDP(itemNum int, i *item) bool {
	if p.proc.nest == 0 {
		log.Printf("ignoring procedure %s without a PROC directive\n", p.symLast)
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

func (p *parser) parseEQU(itemNum int, i *item) bool {
	p.setSym(p.symLast, p.newAsmVal(i.params[0]), i.val[0] != '=')
	return true
}

// text evaluates s as a text string used in a conditional directive.
func (p *parser) text(s string) (string, error) {
	fail := func() (string, error) {
		return "", fmt.Errorf("<text string> or %text_macro required")
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
			return strconv.FormatInt(sym.(asmInt).n, 16), nil
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
	if match && p.ifMatch == p.ifNest {
		p.ifMatch++
	} else {
		p.ifElse++
	}
	p.ifNest++
	return false
}

func (p *parser) evalElseif(directive string, match bool) bool {
	if p.ifNest == 0 {
		log.Println("unmatched", directive)
		return true
	}
	if p.ifMatch == p.ifNest {
		p.ifMatch--
	} else if p.ifMatch == (p.ifNest-1) && p.ifNest == p.ifElse && match {
		p.ifMatch++
		p.ifElse--
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
	mode := strings.ToUpper(i.val) == "IFDEF"
	return p.evalIf(defined == mode)
}

func (p *parser) parseIF(itemNum int, i *item) bool {
	mode := strings.ToUpper(i.val) == "IF"
	return p.evalIf(p.evalBool(i.params[0]) == mode)
}

func (p *parser) parseIFB(itemNum int, i *item) bool {
	mode := strings.ToUpper(i.val) == "IFB"
	ret, err := p.isBlank(i.params[0])
	if err != nil {
		log.Println(err)
		return true
	}
	return p.evalIf(ret == mode)
}

func (p *parser) parseIFIDN(itemNum int, i *item) bool {
	directive := strings.ToUpper(i.val)
	mode := ifidnModeMap[directive]
	ret, err := mode.compareFn(p, i.params[0], i.params[1])
	if err != nil {
		log.Println(err)
		return true
	}
	return p.evalIf(ret == mode.identical)
}

func (p *parser) parseELSEIFDEF(itemNum int, i *item) bool {
	directive := strings.ToUpper(i.val)
	_, defined := p.syms[p.toSymCase(i.params[0])]
	mode := directive == "ELSEIFDEF"
	return p.evalElseif(directive, defined == mode)
}

func (p *parser) parseELSEIF(itemNum int, i *item) bool {
	directive := strings.ToUpper(i.val)
	mode := directive == "ELSEIF"
	return p.evalElseif(directive, p.evalBool(i.params[0]) == mode)
}

func (p *parser) parseELSEIFB(itemNum int, i *item) bool {
	ret, err := p.isBlank(i.params[0])
	if err != nil {
		log.Println(err)
		return true
	}
	directive := strings.ToUpper(i.val)
	mode := directive == "ELSEIFB"
	return p.evalElseif(directive, ret == mode)
}

func (p *parser) parseELSEIFIDN(itemNum int, i *item) bool {
	directive := strings.ToUpper(i.val)
	mode := ifidnModeMap[directive[4:]]
	ret, err := mode.compareFn(p, i.params[0], i.params[1])
	if err != nil {
		log.Println(err)
		return true
	}
	return p.evalElseif(directive, ret == mode.identical)
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
	}
	if p.ifElse == p.ifNest {
		p.ifElse--
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
		p.macro.name = p.symLast
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
	"PROC":       {(*parser).parsePROC, 0},
	"ENDP":       {(*parser).parseENDP, 0},
	".MODEL":     {(*parser).parseMODEL, 1},
	"=":          {(*parser).parseEQU, 1},
	"EQU":        {(*parser).parseEQU, 1},
	"IFDEF":      {(*parser).parseIFDEF, 1},
	"IFNDEF":     {(*parser).parseIFDEF, 1},
	"IF":         {(*parser).parseIF, 1},
	"IFE":        {(*parser).parseIF, 1},
	"IFB":        {(*parser).parseIFB, 1},
	"IFNB":       {(*parser).parseIFB, 1},
	"IFIDN":      {(*parser).parseIFIDN, 2},
	"IFIDNI":     {(*parser).parseIFIDN, 2},
	"IFDIF":      {(*parser).parseIFIDN, 2},
	"IFDIFI":     {(*parser).parseIFIDN, 2},
	"ELSEIFDEF":  {(*parser).parseELSEIFDEF, 1},
	"ELSEIFNDEF": {(*parser).parseELSEIFDEF, 1},
	"ELSEIF":     {(*parser).parseELSEIF, 1},
	"ELSEIFE":    {(*parser).parseELSEIF, 1},
	"ELSEIFB":    {(*parser).parseELSEIFB, 1},
	"ELSEIFNB":   {(*parser).parseELSEIFB, 1},
	"ELSEIFIDN":  {(*parser).parseELSEIFIDN, 2},
	"ELSEIFIDNI": {(*parser).parseELSEIFIDN, 2},
	"ELSEIFDIF":  {(*parser).parseELSEIFIDN, 2},
	"ELSEIFDIFI": {(*parser).parseELSEIFIDN, 2},
	"ELSE":       {(*parser).parseELSE, 0},
	"ENDIF":      {(*parser).parseENDIF, 0},
	"OPTION":     {(*parser).parseOPTION, 1},
	// Macros
	"MACRO":  {(*parser).parseMACRO, 0},
	"FOR":    {(*parser).parseDummyMacro, 1},
	"FORC":   {(*parser).parseDummyMacro, 1},
	"REPT":   {(*parser).parseDummyMacro, 1},
	"REPEAT": {(*parser).parseDummyMacro, 1},
	"WHILE":  {(*parser).parseDummyMacro, 1},
	"IRP":    {(*parser).parseDummyMacro, 2},
	"IRPC":   {(*parser).parseDummyMacro, 2},
	"ENDM":   {(*parser).parseENDM, 0},
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

func (p *parser) eval(i *item) bool {
	if p.syms == nil {
		p.syms = make(symMap)
	}
	if conditionals.matchesInstruction(i) || (p.ifMatch >= p.ifNest) {
		ret := true
		if macros.matchesInstruction(i) || (p.macro.nest == 0) {
			switch i.typ {
			case itemSymbol:
				p.symLast = i.val
			case itemInstruction:
				insFunc, ok := parseFns[strings.ToUpper(i.val)]
				if ok && i.checkMinParams(insFunc.minParams) {
					ret = insFunc.f(p, len(p.instructions), i)
				}
			}
		}
		if ret {
			p.instructions = append(p.instructions, *i)
		}
		return ret
	}
	return false
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
