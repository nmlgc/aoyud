package main

type KeywordType int

const (
	Data        KeywordType = (1 << iota) // Can't appear outside of a segment
	NoStruct                = (1 << iota) // Can't appear inside a STRUC or UNION
	Evaluated               = (1 << iota) // Not kept in the parser's instruction list
	Macro                   = (1 << iota)
	SingleParam             = (1 << iota) // Don't split instruction parameters at commas

	Conditional = (1 << iota) | Evaluated
	Code        = Data | NoStruct
)

// SymRule states whether a keyword can, must, or can't have a symbol name.
type SymRule int

const (
	NotAllowed SymRule = iota
	Optional
	Mandatory
)

type Keyword struct {
	Func       func(p *parser, it *item) ErrorList
	Sym        SymRule
	Type       KeywordType
	ParamRange Range
}

var Keywords map[string]Keyword

func init() {
	req := func(r int) Range {
		return Range{r, r}
	}

	cpu := Keyword{CPU, NotAllowed, 0, req(0)}
	data := Keyword{DATA, Optional, Data | SingleParam, req(1)}
	hll := Keyword{nil, NotAllowed, SingleParam, req(1)}

	Keywords = map[string]Keyword{
		"INCLUDE": {INCLUDE, NotAllowed, Evaluated | SingleParam, req(1)},
		"PROC":    {PROC, Mandatory, Code, Range{0, -1}},
		"ENDP":    {ENDP, Optional, Code, req(0)},
		".MODEL":  {MODEL, NotAllowed, NoStruct, Range{1, 6}},
		// Equates
		"=":       {EQUALS, Mandatory, 0, req(1)},
		"EQU":     {EQU, Mandatory, 0, Range{1, -1}},
		"TEXTEQU": {nil, Mandatory, 0, req(1)}, // TODO
		"TYPEDEF": {nil, Mandatory, 0, req(1)}, // TODO
		"LABEL":   {LABEL, Mandatory, Data, req(1)},
		// Conditionals
		"IFDEF":      {IFDEF, NotAllowed, Conditional, req(1)},
		"IFNDEF":     {IFDEF, NotAllowed, Conditional, req(1)},
		"IF":         {IF, NotAllowed, Conditional, req(1)},
		"IFE":        {IF, NotAllowed, Conditional, req(1)},
		"IFB":        {IFB, NotAllowed, Conditional, req(1)},
		"IFNB":       {IFB, NotAllowed, Conditional, req(1)},
		"IFIDN":      {IFIDN, NotAllowed, Conditional, req(2)},
		"IFIDNI":     {IFIDN, NotAllowed, Conditional, req(2)},
		"IFDIF":      {IFIDN, NotAllowed, Conditional, req(2)},
		"IFDIFI":     {IFIDN, NotAllowed, Conditional, req(2)},
		"ELSEIFDEF":  {ELSEIFDEF, NotAllowed, Conditional, req(1)},
		"ELSEIFNDEF": {ELSEIFDEF, NotAllowed, Conditional, req(1)},
		"ELSEIF":     {ELSEIF, NotAllowed, Conditional, req(1)},
		"ELSEIFE":    {ELSEIF, NotAllowed, Conditional, req(1)},
		"ELSEIFB":    {ELSEIFB, NotAllowed, Conditional, req(1)},
		"ELSEIFNB":   {ELSEIFB, NotAllowed, Conditional, req(1)},
		"ELSEIFIDN":  {ELSEIFIDN, NotAllowed, Conditional, req(2)},
		"ELSEIFIDNI": {ELSEIFIDN, NotAllowed, Conditional, req(2)},
		"ELSEIFDIF":  {ELSEIFIDN, NotAllowed, Conditional, req(2)},
		"ELSEIFDIFI": {ELSEIFIDN, NotAllowed, Conditional, req(2)},
		"ELSE":       {ELSE, NotAllowed, Conditional, req(0)},
		"ENDIF":      {ENDIF, NotAllowed, Conditional, req(0)},
		"OPTION":     {OPTION, NotAllowed, 0, Range{1, -1}},
		// Macros
		"MACRO":  {MACRO, Mandatory, Macro, Range{0, -1}},
		"FOR":    {DummyMacro, NotAllowed, Macro, req(2)},
		"FORC":   {DummyMacro, NotAllowed, Macro, Range{1, -1}}, // see JWasm's FORC.ASM
		"REPT":   {DummyMacro, NotAllowed, Macro, req(1)},
		"REPEAT": {DummyMacro, NotAllowed, Macro, req(1)},
		"WHILE":  {DummyMacro, NotAllowed, Macro, req(1)},
		"IRP":    {DummyMacro, NotAllowed, Macro, req(2)},
		"IRPC":   {DummyMacro, NotAllowed, Macro, req(2)},
		"ENDM":   {ENDM, NotAllowed, Macro, req(0)},
		// CPUs
		".8086": cpu, "P8086": cpu,
		".186": cpu, "P186": cpu,
		".286": cpu, "P286": cpu,
		".286C": cpu, "P286N": cpu,
		".286P": cpu, "P286P": cpu,
		".386": cpu, "P386": cpu,
		".386C": cpu, "P386N": cpu,
		".386P": cpu, "P386P": cpu,
		".486": cpu, "P486": cpu,
		".486C": cpu, "P486N": cpu,
		".486P": cpu, // [sic], there is no P486P
		".586":  cpu, "P586": cpu,
		".586C": cpu, "P586N": cpu,
		".586P": cpu, // ditto
		".686":  cpu, "P686": cpu,
		".686P": cpu, // ditto, and no .686C and P686N either
		".X64":  cpu,
		".X64P": cpu,
		// FPUs
		".8087": cpu, "P8087": cpu,
		".287": cpu, "P287": cpu,
		".387": cpu, "P387": cpu,
		// TASM also has .487 and .587, but those FPUs don't seem to have
		// added anything relevant. In fact, neither MASM nor JWasm
		// support those directives.

		// Segments
		"SEGMENT": {SEGMENT, Mandatory, NoStruct, Range{0, 1}},
		"ENDS":    {ENDS, Optional, 0, req(0)},
		"GROUP":   {nil, Mandatory, 0, req(1)}, // TODO
		// Data allocations
		"DB": data,
		"DW": data,
		"DD": data,
		"DQ": data,
		"DF": data,
		"DP": data,
		"DT": data,
		// Structures
		"STRUCT": {STRUC, Optional, 0, Range{0, 2}}, // Yes, it's possible to have
		"STRUC":  {STRUC, Optional, 0, Range{0, 2}}, // unnamed structures and
		"UNION":  {STRUC, Optional, 0, Range{0, 2}}, // unions inside named ones.
		// String functions (all TODO)
		"CATSTR":  {nil, Mandatory, 0, Range{1, -1}},
		"SIZESTR": {nil, Mandatory, 0, req(1)},
		"INSTR":   {nil, Mandatory, 0, Range{2, 3}},
		"SUBSTR":  {nil, Mandatory, 0, Range{2, 3}},
		// High-level language directives (all TODO)
		".IF":       hll,
		".ELSE":     hll,
		".ELSEIF":   hll,
		".ENDIF":    hll,
		".REPEAT":   hll,
		".UNTIL":    hll,
		".UNTILCXZ": hll,
		".WHILE":    hll,
		".BREAK":    hll,
		".CONTINUE": hll,
		".ENDW":     hll,
	}
}
