// Assembly symbol map.

package main

import (
	"strings"
)

type Symbol struct {
	Constant bool
	Val      asmVal
}

func (s Symbol) String() string {
	var ret string
	if s.Constant {
		ret = "(const) "
	}
	return ret + s.Val.String() + "\n"
}

type SymMap struct {
	Map           map[string]Symbol
	CaseSensitive bool
}

func (s *SymMap) ToSymCase(str string) string {
	if !s.CaseSensitive {
		return strings.ToUpper(str)
	}
	return str
}

func NewSymMap() *SymMap {
	return &SymMap{Map: make(map[string]Symbol)}
}
