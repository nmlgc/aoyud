// Assembly string literal handling.

package main

import (
	"strconv"
	"strings"
)

// asmString represents a string literal.
type asmString string

const maxbytes = 8

func (v asmString) Thing() string {
	return "string constant"
}

func (v asmString) width() uint {
	return uint(len(v))
}

func (v asmString) String() string {
	return strconv.Quote(string(v))
}

func (v asmString) Int(wordsize uint) (asmInt, ErrorList) {
	ret := asmInt{base: 255}
	if wordsize < 1 || wordsize > maxbytes {
		wordsize = maxbytes
	}
	if uint(len(v)) > wordsize {
		return ret, ErrorListF(ESError,
			"string constant larger than %d bytes: %s", wordsize, v,
		)
	}
	for i := 0; i < len(v); i++ {
		ret.n |= int64(byte(v[len(v)-1-i])) << uint(i*8)
	}
	return ret, nil
}

func (v asmString) Emit(width uint) []byte {
	return []byte(v)
}

func (v asmInt) formatASCII() string {
	ret := make([]byte, maxbytes)
	rest := v.n
	for i := 0; i < maxbytes; i++ {
		ret[maxbytes-1-i] = byte(rest & 0xFF)
		rest >>= 8
	}
	return strings.TrimLeft(string(ret), "\x00")
}

func quoteASCII(str string) string {
	// Yes, since there is no escaping in assembly string literals,
	// it's impossible to have both.
	if strings.IndexRune(str, '"') == -1 {
		return "\"" + str + "\""
	} else {
		return "'" + str + "'"
	}
}
