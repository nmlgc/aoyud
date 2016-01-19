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

type MemoryModel uint8

const (
	FarCode    MemoryModel = (1 << iota)
	FarData                = (1 << iota)
	HugeData               = (1 << iota)
	CSInDGroup             = (1 << iota)
	Turbo                  = (1 << iota) // Indicates the TPASCAL model
	Flat                   = (1 << iota)

	Tiny    = CSInDGroup
	Small   = 0
	Compact = FarData
	Medium  = FarCode
	Large   = FarData | FarCode
	Huge    = HugeData | FarCode
	TPascal = Turbo | Compact
	TCHuge  = Flat | Huge
)

// InternalSyms contains all internal symbols that can't be overwritten
// through the normal symbol map. Pointer values are undefined at first.
type InternalSyms struct {
	FileName   asmExpression
	FileName8  asmString
	StackGroup *asmExpression
	ThirtyTwo  *uint8
	Model      *MemoryModel
	Interface  *uint8
	CPU        cpuFlag
	WordSize   uint8
	// We keep those in addition to the MemoryModel value. Auto-generating
	// them from Model is not worth the hassle, especially because of the
	// different value for FLAT in TASM and MASM.
	SymModel    *uint8
	SymCodeSize *uint8
	SymDataSize *uint8
}

// Lookup maps the members of s to their symbol names and returns their values
// as asmVal types.
func (s *InternalSyms) Lookup(name string) (asmVal, bool) {
	if s == nil {
		return nil, false
	}
	var num **uint8

	// This isn't actually what either TASM or JWasm do, but accepting both
	// real and uppercase seems the most sensible option that still allows
	// custom spellings to be used for user-defined symbols together with
	// OPTION CASEMAP:NONE.
	switch name {
	case "??filename", "??FILENAME":
		return s.FileName8, true
	case "@32Bit", "@32BIT":
		num = &s.ThirtyTwo
	case "@CodeSize", "@CODESIZE":
		num = &s.SymCodeSize
	case "@Cpu", "@CPU":
		return asmInt{n: int64(s.CPU), base: 2}, true
	case "@DataSize", "@DATASIZE":
		num = &s.SymDataSize
	case "@FileName", "@FILENAME":
		return s.FileName, true
	case "@Interface", "@INTERFACE":
		num = &s.Interface
	case "@Model", "@MODEL":
		num = &s.SymModel
	case "@stack", "@STACK":
		if s.StackGroup == nil {
			return nil, true
		}
		return *s.StackGroup, true
	case "@WordSize", "@WORDSIZE":
		return asmInt{n: int64(s.WordSize)}, true
	}
	if num == nil {
		return nil, false
	}
	if *num == nil {
		return nil, true
	}
	return asmInt{n: int64(**num)}, true
}

func (s InternalSyms) SegmentWordSize() uint8 {
	// @32BIT is only set in TASM mode, which can't be used to compile 64-bit
	// code anyway, so I guess this is fine?
	if s.ThirtyTwo != nil {
		return 2 + (*s.ThirtyTwo * 2)
	}
	return s.WordSize
}

type SymMap struct {
	Map           map[string]Symbol
	Internals     *InternalSyms
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
	if ret, ok := s.Internals.Lookup(realName); ok {
		return ret, nil
	} else if ret, ok := s.Map[realName]; ok {
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
	if _, ok := s.Internals.Lookup(realName); ok {
		return ErrorListF(ESError,
			"can't overwrite internal symbol: %s", realName,
		)
	} else if existing := s.Map[realName]; existing.Val != nil {
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
func NewSymMap(caseSensitive *bool, internals *InternalSyms) *SymMap {
	return &SymMap{
		Map:           make(map[string]Symbol),
		CaseSensitive: caseSensitive,
		Internals:     internals,
	}
}
