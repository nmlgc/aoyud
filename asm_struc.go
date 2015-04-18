// Parsing of assembly structures and unions.

package main

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

func (v asmStruc) Thing() string {
	if v.flag == sUnion {
		return "union"
	}
	return "structure"
}

func (v asmStruc) OpenThing() string  { return "open structure" }
func (v asmStruc) OpenThings() string { return "open structures" }

func (v asmStruc) Prev() Nestable {
	if v.prev != nil {
		return v.prev
	}
	return nil
}

func (v asmStruc) Name() string {
	if v.name == "" {
		return "(unnamed)"
	}
	return v.name
}

func (v asmStruc) String() string {
	if v.flag == sUnion {
		return "UNION"
	}
	return "STRUC"
}

func (v asmStruc) width() uint {
	return 0
}

func STRUC(p *parser, it *item) *ErrorList {
	// Top-level structures require a symbol name *before* the directive.
	// On the other hand, nested structures can *optionally* have a
	// symbol name *after* the directive. Yes, it's stupid.
	var err *ErrorList
	sym := it.sym
	if p.struc != nil {
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
	struc := asmStruc{
		name: sym,
		flag: sStruc,
		prev: p.struc,
	}
	if it.val == "UNION" {
		struc.flag = sUnion
	}
	if p.struc == nil && struc.name != "" {
		err = p.syms.Set(struc.name, struc, true)
	}
	p.struc = &struc
	return err
}
