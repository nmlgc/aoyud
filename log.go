// Custom logger for printing error lists together with the filename and line
// number of the originating code, implemented on top of Go's own log package.

package main

import (
	"log"
	"os"
	"strings"
)

type printlnFn func(*log.Logger, ...interface{})

var codeLogger = log.New(os.Stderr, "", 0)

func (p *ItemPos) ErrorPrint(err *ErrorList) {
	if err != nil {
		for _, e := range *err {
			fn := codeLogger.Println
			if e.sev == ESFatal {
				fn = codeLogger.Fatalln
			}

			var posstr string
			sevstr := e.sev.String()
			if e.pos != nil {
				posstr = e.pos.String()
			} else {
				posstr = p.String()
			}
			posstr = strings.Replace(
				posstr, "\n", "\n"+strings.Repeat(" ", len(sevstr)), -1,
			)
			fn(sevstr + posstr + e.s)
		}
	}
}
