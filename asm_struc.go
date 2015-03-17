// Parsing of assembly structures and unions.

package main

import "log"

// strucFlag denotes whether a nesting level is a structure or union.
type strucFlag bool

const (
	sUnion strucFlag = false
	sStruc           = true
)

type asmStruc struct {
	name string
	flag strucFlag
	prev *asmStruc
}

// Name returns a friendly name of v.
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
	return typ
}

func (v asmStruc) width() uint {
	return 0
}

// sprintOpen returns an "open structure" error message for v and all previous
// nested structures.
func (v asmStruc) sprintOpen() string {
	str := "open structure: "
	if v.prev != nil {
		str = "open structures: "
	}
	str += v.Name()
	for p := v.prev; p != nil; p = p.prev {
		str += " <- " + p.Name()
	}
	return str
}

func (p *parser) parseSTRUC(itemNum int, i *item) bool {
	// Top-level structures require a symbol name *before* the directive.
	// On the other hand, nested structures can *optionally* have a
	// symbol name *after* the directive. Yes, it's stupid.
	sym := i.sym
	if p.struc != nil {
		if i.sym != "" {
			log.Printf(
				"name of nested structure must come after %s: %s",
				i.val, i.sym,
			)
			return true
		} else if len(i.params) > 0 {
			sym = i.params[0]
		}
	} else if i.missingRequiredSym() {
		return true
	}
	struc := &asmStruc{
		name: p.toSymCase(sym),
		flag: sStruc,
		prev: p.struc,
	}
	if i.val == "UNION" {
		struc.flag = sUnion
	}
	if p.struc == nil && struc.name != "" {
		p.setSym(struc.name, struc, true)
	}
	p.struc = struc
	return true
}
