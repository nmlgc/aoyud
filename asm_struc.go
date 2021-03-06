// Parsing of assembly structures and unions.

package main

import (
	"fmt"
)

// strucFlag denotes whether a nesting level is a structure or union.
type strucFlag bool

const (
	sUnion strucFlag = false
	sStruc           = true
)

type asmStruc struct {
	name    string
	flag    strucFlag
	data    BlobList
	members SymMap
}

func (v asmStruc) Thing() string {
	if v.flag == sUnion {
		return "union"
	}
	return "structure"
}

func (v asmStruc) OpenThing() string  { return "open structure" }
func (v asmStruc) OpenThings() string { return "open structures" }
func (v asmStruc) Unclosed() bool     { return false }

func (v asmStruc) Name() string {
	if v.name == "" {
		return "(unnamed)"
	}
	return v.name
}

func (v asmStruc) String() string {
	typ := "STRUC"
	if v.flag == sUnion {
		typ = "UNION"
	}
	return fmt.Sprintf("%s (%d bytes)\n%s", typ, v.Width(), v.data.Dump(1))
}

func (v asmStruc) Width() uint {
	return uint(len(v.data))
}

func (v asmStruc) Len() uint {
	return v.Width()
}

func (v *asmStruc) AddData(ptr *asmPtr, data Emittable) (err ErrorList) {
	if v.flag == sUnion && v.Width() > 0 {
		bytes := data.Emit()
		for i := range bytes {
			if bytes[i] != 0 {
				err = err.AddF(ESWarning,
					"ignoring default value for union member beyond the first",
				)
				break
			}
		}
		v.data = v.data.Expand(ptr, 0, data.Len())
	} else {
		v.data = v.data.Append(ptr, data)
	}
	return err
}

func (v asmStruc) Emit() []byte {
	return v.data.Emit()
}

func (v *asmStruc) Offset() (chunk uint, off uint64) {
	if v.flag == sStruc {
		off = uint64(len(v.data))
	}
	return 0, off
}

func (v *asmStruc) AddPointer(p *parser, sym string, ptr asmDataPtr) (err ErrorList) {
	if len(p.strucs) == 1 && p.syntax == "TASM" {
		err = p.syms.Set(sym, ptr, true)
	}
	return err.AddL(v.members.Set(sym, ptr, true))
}

func (v asmStruc) WordSize() uint8 {
	ret := uint8(0)
	for w := v.Width(); w > 0; w >>= 8 {
		ret++
	}
	return ret
}

func STRUC(p *parser, it *item) (err ErrorList) {
	// Top-level structures require a symbol name *before* the directive.
	// On the other hand, nested structures can *optionally* have a
	// symbol name *after* the directive. Yes, it's stupid.
	sym := it.sym
	if len(p.strucs) >= 1 {
		if it.sym != "" {
			return ErrorListF(ESError,
				"name of nested structure must come after %s: %s",
				it.val, it.sym,
			)
		} else if len(it.params) > 0 {
			sym = it.params[0]
		}
	} else if err := it.missingRequiredSym(); err != nil {
		return err
	}
	struc := &asmStruc{
		name:    sym,
		flag:    sStruc,
		members: *NewSymMap(&p.caseSensitive, nil),
	}
	if it.val == "UNION" {
		struc.flag = sUnion
	}
	p.strucs = append(p.strucs, struc)
	return err
}
