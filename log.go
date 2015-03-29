// Custom logger for printing error lists together with the filename and line
// number of the originating code, implemented on top of Go's own log package.

package main

import (
	"log"
	"os"
)

type printlnFn func(*log.Logger, ...interface{})

var codeLogger = log.New(os.Stderr, "", 0)

func (p *ItemPos) callPrintln(fn printlnFn, err *ErrorList) {
	if err != nil {
		for _, e := range *err {
			if e.pos != nil {
				fn(codeLogger, e.pos.String()+e.s)
			} else {
				fn(codeLogger, p.String()+e.s)
			}
		}
	}
}

func (p *ItemPos) ErrorFatal(err *ErrorList) {
	p.callPrintln((*log.Logger).Fatalln, err)
}

func (p *ItemPos) ErrorPrint(err *ErrorList) {
	p.callPrintln((*log.Logger).Println, err)
}
