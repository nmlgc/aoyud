// Assembly syntax parser.

package main

import (
	"fmt"
	"log"
	"strings"
)

type asmVal fmt.Stringer

type asmInt int
type asmBytes []byte

func (v asmInt) String() string {
	return fmt.Sprintf("%d", int(v))
}

func (v asmBytes) String() string {
	return fmt.Sprintf("\"%s\"", []byte(v))
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

type parser struct {
	instructions []item
	// General state
	syntax  string
	syms    symMap
	symLast string // last symbol declaration encountered
	// Open procedures
	procStart int
	procNest  int
	procName  string
	// Conditionals
	ifNest  int
	ifMatch int
}

// parseFn represents a function handling a certain instruction or directive
// at parsing time. The return value indicates whether the instruction should
// stay in the parser's instruction list.
type parseFn struct {
	f         func(p *parser, itemNum int, i *item) bool
	minParams int
}

func (p *parser) parsePROC(itemNum int, i *item) bool {
	if p.procNest == 0 {
		p.procName = p.symLast
		p.procStart = itemNum
	} else {
		log.Printf("ignoring nested procedure %s\n", p.symLast)
	}
	p.procNest++
	return true
}

func (p *parser) parseENDP(itemNum int, i *item) bool {
	if p.procNest == 0 {
		log.Printf("ignoring procedure %s without a PROC directive\n", p.symLast)
	} else if p.procNest == 1 {
		log.Printf(
			"found procedure %s ranging from lex items #%d-#%d\n",
			p.procName, p.procStart, itemNum,
		)
	}
	p.procNest--
	return true
}

func (p *parser) parseMODEL(itemNum int, i *item) bool {
	// modelSym defines the @Model value for each memory model.
	var modelSym = map[string]asmInt{
		"TINY":  1,
		"SMALL": 2,
		// Yes, the TASM manual is actually wrong here.
		// For MASM, this is changed to 7.
		"FLAT":    1,
		"COMPACT": 3,
		"MEDIUM":  4,
		"LARGE":   5,
		"HUGE":    6,
		"TCHUGE":  7,
		"TPASCAL": 0,
	}

	// modelCodeSize defines the @CodeSize value for each memory model.
	var modelCodeSize = map[string]asmInt{
		"TINY":    0,
		"SMALL":   0,
		"COMPACT": 0,
		"MEDIUM":  1,
		"LARGE":   1,
		"HUGE":    1,
		"TCHUGE":  1,
		"TPASCAL": 0,
		"FLAT":    0,
	}

	// modelDataSize defines the @DataSize value for each memory model.
	var modelDataSize = map[string]asmInt{
		"TINY":    0,
		"SMALL":   0,
		"COMPACT": 1,
		"MEDIUM":  0,
		"LARGE":   1,
		"HUGE":    2,
		"TCHUGE":  2,
		"TPASCAL": 1,
		"FLAT":    0,
	}

	// interfaceSym defines values for the @Interface symbol
	var interfaceSym = map[string]asmInt{
		"NOLANGUAGE": 0,
		"C":          1,
		"SYSCALL":    2,
		"STDCALL":    3,
		"PASCAL":     4,
		"FORTRAN":    5,
		"BASIC":      6,
		"FASTCALL":   7, // MASM only
		"PROLOG":     7,
		"CPP":        8,
	}

	paramCount := len(i.params)
	model := strings.ToUpper(string(i.params[0]))
	if modelVal, ok := modelSym[model]; ok {
		if p.syntax == "MASM" && model == "FLAT" {
			modelVal = 7
		}
		p.syms.set("@MODEL", modelVal, false)
		p.syms.set("@CODESIZE", modelCodeSize[model], false)
		p.syms.set("@DATASIZE", modelDataSize[model], false)
	} else {
		log.Printf("invalid memory model: %s\n", model)
	}
	if paramCount > 1 {
		language := strings.ToUpper(string(i.params[1]))
		if interfaceVal, ok := interfaceSym[language]; ok {
			p.syms.set("@INTERFACE", interfaceVal, false)
		} else {
			log.Printf("invalid language: %s\n", language)
		}
	} else {
		p.syms.set("@INTERFACE", interfaceSym["NOLANGUAGE"], false)
	}
	return true
}

func (p *parser) parseEQU(itemNum int, i *item) bool {
	p.syms.set(p.symLast, asmBytes(i.params[0]), i.val[0] != '=')
	return true
}

func (p *parser) parseIFDEF(itemNum int, i *item) bool {
	_, defined := p.syms[string(i.params[0])]
	mode := strings.ToUpper(i.val) == "IFDEF"
	if defined == mode && p.ifMatch == p.ifNest {
		p.ifMatch++
	}
	p.ifNest++
	return false
}

func (p *parser) parseELSE(itemNum int, i *item) bool {
	if p.ifNest == 0 {
		log.Println("unmatched ELSE")
		return true
	} else if p.ifMatch == p.ifNest {
		p.ifMatch--
	} else if p.ifMatch == (p.ifNest - 1) {
		p.ifMatch++
	}
	return false
}

func (p *parser) parseENDIF(itemNum int, i *item) bool {
	if p.ifNest == 0 {
		log.Println("found ENDIF without a matching condition")
		return true
	}
	if p.ifMatch == p.ifNest {
		p.ifMatch--
	}
	p.ifNest--
	return false
}

var parseFns = map[string]parseFn{
	"PROC":   {(*parser).parsePROC, 0},
	"ENDP":   {(*parser).parseENDP, 0},
	".MODEL": {(*parser).parseMODEL, 1},
	"=":      {(*parser).parseEQU, 1},
	"EQU":    {(*parser).parseEQU, 1},
	"IFDEF":  {(*parser).parseIFDEF, 1},
	"IFNDEF": {(*parser).parseIFDEF, 1},
	"ELSE":   {(*parser).parseELSE, 0},
	"ENDIF":  {(*parser).parseENDIF, 0},
}

func (m *symMap) set(name string, val asmVal, constant bool) bool {
	// TODO: Enforce constness for EQU while making sure that the cases in
	// JWasm's EQUATE6.ASM still work.
	if existing := (*m)[name]; existing.constant {
		log.Printf("constant symbol %s already defined elsewhere", name)
		return false
	}
	(*m)[name] = symbol{val: val, constant: constant}
	return true
}

func (p *parser) eval(i *item) bool {
	if p.syms == nil {
		p.syms = make(symMap)
	}
	evalCond := i.typ == itemInstruction && conditionals.matches([]byte(i.val))
	if evalCond || (p.ifMatch >= p.ifNest) {
		ret := true
		switch i.typ {
		case itemSymbol:
			p.symLast = i.val
		case itemInstruction:
			insFunc, ok := parseFns[strings.ToUpper(i.val)]
			if ok && i.checkMinParams(insFunc.minParams) {
				ret = insFunc.f(p, len(p.instructions), i)
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
	if p.procNest != 0 {
		log.Printf("ignoring procedure %s without an ENDP directive\n", p.procName)
	}
	if len(p.syms) > 0 {
		log.Println("Symbols:", p.syms)
	}
}
