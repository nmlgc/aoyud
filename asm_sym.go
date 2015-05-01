// Assembly symbol map.

package main

import (
	"fmt"
	"reflect"
	"sort"
	"strings"
)

type Symbol struct {
	Constant bool // Constness of the stored value.
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
	CaseSensitive *bool
}

func (s SymMap) String() (ret string) {
	var keys []string
	for i := range s.Map {
		keys = append(keys, i)
	}
	sort.Strings(keys)
	ret += "Symbols: [\n"
	for _, k := range keys {
		ret += fmt.Sprintf("• %s: %s", k, s.Map[k])
	}
	return ret + "]"
}

func (s *SymMap) ToSymCase(str string) string {
	if !(*s.CaseSensitive) {
		return strings.ToUpper(str)
	}
	return str
}

// Equal returns whether s1 and s2 are equal according to the case sensitivity
// setting of s.
func (s *SymMap) Equal(s1 string, s2 string) bool {
	if !(*s.CaseSensitive) {
		return strings.EqualFold(s1, s2)
	}
	return s1 == s2
}

// Lookup wraps Go's own map lookup using the case sensitivity setting of s.
// It returns the value of the symbol or nil if it doesn't exist in s,
// together with a possible error.
func (s *SymMap) Lookup(name string) (asmVal, ErrorList) {
	realName := s.ToSymCase(name)
	if ret, ok := s.Map[realName]; ok {
		var err ErrorList
		if !(*s.CaseSensitive) && name != realName {
			if _, ok := s.Map[name]; ok {
				err = ErrorListF(ESWarning,
					"symbol name is ambiguous due to reactivated case mapping; picking %s, not %s",
					realName, name,
				)
			}
		}
		return ret.Val, err
	}
	return nil, nil
}

// Get returns the value of a symbol that is meant to exist in s, or an error
// if it doesn't.
func (s *SymMap) Get(name string) (asmVal, ErrorList) {
	if ret, err := s.Lookup(name); ret != nil {
		return ret, err
	}
	return nil, ErrorListF(ESError, "unknown symbol: %s", name)
}

// Set tries to add a new symbol with the given name and value to s, while
// taking the constness of a possible existing value with the same name into
// account. If name is empty, the function does nothing.
func (s *SymMap) Set(name string, val asmVal, constant bool) ErrorList {
	if name == "" {
		return nil
	}
	// Maybe the asmVal interface should have received a Equal()
	// method, but given the fact that most types are constant anyway…
	redefinable := func(a, b asmVal) bool {
		switch a.(type) {
		case asmInt:
			a, b := a.(asmInt), b.(asmInt)
			return a.n == b.n && a.ptr == b.ptr
		case asmDataPtr:
			a, b := a.(asmDataPtr), b.(asmDataPtr)
			return a.seg == b.seg && a.off == b.off && a.w == b.w
		}
		return false
	}

	realName := s.ToSymCase(name)
	if existing := s.Map[realName]; existing.Val != nil {
		fail := func() (err ErrorList) {
			err = err.AddF(ESError,
				"symbol already defined as %s: %s",
				existing.Val.Thing(), realName,
			)
			return err.AddF(ESError,
				"\t(previous value: %s)", existing.Val.String(),
			)
		}
		if reflect.TypeOf(existing.Val) != reflect.TypeOf(val) {
			return fail()
		} else if existing.Constant && !redefinable(existing.Val, val) {
			return fail()
		}
	}
	s.Map[realName] = Symbol{Val: val, Constant: constant}
	return nil
}

// NewSymMap creates a new symbol map whose case sensitivity can be controlled
// through the given pointer.
func NewSymMap(caseSensitive *bool) *SymMap {
	return &SymMap{Map: make(map[string]Symbol), CaseSensitive: caseSensitive}
}
