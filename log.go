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

// Print pretty-prints the given error list.
func (e *ErrorList) Print() {
	if e == nil {
		return
	}
	for _, err := range *e {
		fn := codeLogger.Println
		if err.sev == ESFatal {
			fn = codeLogger.Fatalln
		}
		sevstr := err.sev.String()
		posstr := strings.Replace(
			err.pos.String(), "\n", "\n"+strings.Repeat(" ", len(sevstr)), -1,
		)
		fn(sevstr + posstr + err.s)
	}
}
