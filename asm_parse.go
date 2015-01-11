// Assembly syntax parser.

package main

import (
	"log"
	"strings"
)

type symMap map[string]int

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
}

// parseFn represents a function handling a certain instruction or directive.
type parseFn struct {
	f         func(p *parser, itemNum int, i *item)
	minParams int
}

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

func (p *parser) parseMODEL(itemNum int, i *item) {
	// modelSym defines the @Model value for each memory model.
	var modelSym = map[string]int{
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
	var modelCodeSize = map[string]int{
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
	var modelDataSize = map[string]int{
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
	var interfaceSym = map[string]int{
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
		p.syms["@MODEL"] = modelVal
		p.syms["@CODESIZE"] = modelCodeSize[model]
		p.syms["@DATASIZE"] = modelDataSize[model]
	} else {
		log.Printf("invalid memory model: %s\n", model)
	}
	if paramCount > 1 {
		language := strings.ToUpper(string(i.params[1]))
		if interfaceVal, ok := interfaceSym[language]; ok {
			p.syms["@INTERFACE"] = interfaceVal
		} else {
			log.Printf("invalid language: %s\n", language)
		}
	} else {
		p.syms["@INTERFACE"] = interfaceSym["NOLANGUAGE"]
	}
}

var parseFns = map[string]parseFn{
	"PROC":   {(*parser).parsePROC, 0},
	"ENDP":   {(*parser).parseENDP, 0},
	".MODEL": {(*parser).parseMODEL, 1},
}

func (p *parser) eval(i *item) {
	if p.syms == nil {
		p.syms = make(symMap)
	}
	itemNum := len(p.instructions)
	valString := string(i.val)

	p.instructions = append(p.instructions, *i)
	switch i.typ {
	case itemSymbol:
		p.symLast = valString
	case itemInstruction:
		valUpper := strings.ToUpper(valString)
		if insFunc, ok := parseFns[valUpper]; ok {
			if paramCount := len(i.params); paramCount < insFunc.minParams {
				log.Printf(
					"%s requires at least %d parameters, %d given\n",
					valUpper, insFunc.minParams, paramCount,
				)
			} else {
				insFunc.f(p, itemNum, i)
			}
		}
	}
}

func (p *parser) end() {
	if p.procNest != 0 {
		log.Printf("ignoring procedure %s without an ENDP directive\n", p.procName)
	}
	if len(p.syms) > 0 {
		log.Println("Symbols:", p.syms)
	}
}
