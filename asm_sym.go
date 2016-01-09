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

// Dump returns a string listing all symbols in s in alphabetical order,
// together with their values, indented with the given number of tabs.
func (s SymMap) Dump(indent int) (ret string) {
	if len(s.Map) == 0 {
		return ""
	}
	var keys []string
	for i := range s.Map {
		keys = append(keys, i)
	}
	sort.Strings(keys)
	for _, k := range keys {
		ret += fmt.Sprintf(
			"%s• %s: %s", strings.Repeat("\t", indent), k, s.Map[k],
		)
	}
	return ret[:len(ret)-1]
}

func (s SymMap) String() (ret string) {
	return s.Dump(0)
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

// GetSegment returns a pointer to the segment with the given name, or tries
// to create the segment if it doesn't exist yet.
func (s *SymMap) GetSegment(name string) (*asmSegment, ErrorList) {
	val, err := s.Lookup(name)
	if val != nil {
		switch val.(type) {
		case *asmSegment:
			return val.(*asmSegment), err
		default:
			// We'll have SymMap.Set handle this error message.
		}
	}
	cpuWordSize := uint8(s.Map["@WORDSIZE"].Val.(asmInt).n) // should never fail
	seg := &asmSegment{name: name, wordsize: cpuWordSize}
	return seg, err.AddL(s.Set(name, seg, false))
}

// GetGroup returns a pointer to the group with the given name, or tries to
// create the group if it doesn't exist yet.
func (s *SymMap) GetGroup(name string) (*asmGroup, ErrorList) {
	val, err := s.Lookup(name)
	if val != nil {
		switch val.(type) {
		case *asmGroup:
			return val.(*asmGroup), err
		default:
			// We'll have SymMap.Set handle this error message.
		}
	}
	group := &asmGroup{name: name}
	return group, err.AddL(s.Set(name, group, false))
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
		redefinableVal := func(a, b asmVal) bool {
			switch a.(type) {
			case asmInt:
				a, b := a.(asmInt), b.(asmInt)
				return a.n == b.n && a.ptr == b.ptr
			case asmDataPtr:
				a, b := a.(asmDataPtr), b.(asmDataPtr)
				// TODO: Temporary kludge to keep pointers working while we're
				// migrating to a smarter pass system.
				if a.off == 0 {
					return true
				}
				return a.et.Name() == b.et.Name() &&
					a.chunk == b.chunk &&
					a.off == b.off &&
					a.ptr.unit.Width() == b.ptr.unit.Width()
			}
			return false
		}
		switch a.(type) {
		case asmStruc:
			a, b := a.(asmStruc), b.(asmStruc)
			ret := a.flag == b.flag && len(a.data) == len(b.data)
			for i, valB := range b.members.Map {
				valA, ok := a.members.Map[i]
				ret = ret && ok &&
					(reflect.TypeOf(valA.Val) == reflect.TypeOf(valB.Val))

				switch valA.Val.(type) {
				case asmStruc: // do nothing
				default:
					ret = ret && redefinableVal(valA.Val, valB.Val)
				}
			}
			return ret
		}
		return redefinableVal(a, b)
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
