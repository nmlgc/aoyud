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

// Get returns the value of a symbol that is meant to exist in the map, or an
// error if it doesn't.
func (s *SymMap) Get(name string) (asmVal, *ErrorList) {
	realName := s.ToSymCase(name)
	if ret, ok := s.Map[realName]; ok {
		return ret.Val, nil
	}
	return nil, ErrorListF("unknown symbol: %s", realName)
}

func (s *SymMap) Set(name string, val asmVal, constant bool) *ErrorList {
	// TODO: Enforce constness for EQU while making sure that the cases in
	// JWasm's EQUATE6.ASM still work.
	realName := s.ToSymCase(name)
	if existing := s.Map[realName]; existing.Constant {
		return ErrorListF(
			"constant symbol already defined elsewhere: %s", realName,
		)
	}
	s.Map[realName] = Symbol{Val: val, Constant: constant}
	return nil
}

func NewSymMap() *SymMap {
	return &SymMap{Map: make(map[string]Symbol)}
}
