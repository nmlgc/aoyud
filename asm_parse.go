// Assembly syntax parser.

package main

import (
	"log"
	"strings"
)

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
