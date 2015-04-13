package main

type KeywordType int

const (
	EmitData    KeywordType = (1 << iota) // Emits data into the program image
	EmitCode                = (1 << iota) // Emits code into the program image
	CodeBlock               = (1 << iota) // Can't appear inside a STRUC or UNION
	Conditional             = (1 << iota) // Not kept in the parser's instruction list
	Macro                   = (1 << iota)

	Emit = EmitData | EmitCode
)

// SymRule states whether a keyword can, must, or can't have a symbol name.
type SymRule int

const (
	NotAllowed SymRule = iota
	Optional
	Mandatory
)

type Keyword struct {
	Lex        func(l *lexer, it *item) *ErrorList  // Function to run at lexing time.
	Parse      func(p *parser, it *item) *ErrorList // Function to run at parsing time.
	Type       KeywordType
	Sym        SymRule
	ParamRange Range
}

var Keywords map[string]Keyword

func init() {
	req := func(r int) Range {
		return Range{r, r}
	}

	cpu := Keyword{Parse: CPU, ParamRange: req(0)}
	data := Keyword{Parse: DATA, Sym: Optional, Type: EmitData, ParamRange: Range{1, -1}}

	Keywords = map[string]Keyword{
		"INCLUDE": {Lex: INCLUDE, ParamRange: req(1)},
		"PROC":    {Parse: PROC, Sym: Mandatory, Type: CodeBlock, ParamRange: Range{0, -1}},
		"ENDP":    {Parse: ENDP, Sym: Optional, Type: CodeBlock, ParamRange: req(0)},
		".MODEL":  {Parse: MODEL, Type: CodeBlock, ParamRange: Range{1, 6}},
		// Equates
		"=":       {Parse: EQUALS, Sym: Mandatory, ParamRange: req(1)},
		"EQU":     {Parse: EQU, Sym: Mandatory, ParamRange: Range{1, -1}},
		"TEXTEQU": {Sym: Mandatory, ParamRange: req(1)}, // TODO
		"TYPEDEF": {Sym: Mandatory, ParamRange: req(1)}, // TODO
		"LABEL":   {Parse: LABEL, Sym: Mandatory, ParamRange: req(1)},
		// Conditionals
		"IFDEF":      {Parse: IFDEF, Type: Conditional, ParamRange: req(1)},
		"IFNDEF":     {Parse: IFDEF, Type: Conditional, ParamRange: req(1)},
		"IF":         {Parse: IF, Type: Conditional, ParamRange: req(1)},
		"IFE":        {Parse: IF, Type: Conditional, ParamRange: req(1)},
		"IFB":        {Parse: IFB, Type: Conditional, ParamRange: req(1)},
		"IFNB":       {Parse: IFB, Type: Conditional, ParamRange: req(1)},
		"IFIDN":      {Parse: IFIDN, Type: Conditional, ParamRange: req(2)},
		"IFIDNI":     {Parse: IFIDN, Type: Conditional, ParamRange: req(2)},
		"IFDIF":      {Parse: IFIDN, Type: Conditional, ParamRange: req(2)},
		"IFDIFI":     {Parse: IFIDN, Type: Conditional, ParamRange: req(2)},
		"ELSEIFDEF":  {Parse: ELSEIFDEF, Type: Conditional, ParamRange: req(1)},
		"ELSEIFNDEF": {Parse: ELSEIFDEF, Type: Conditional, ParamRange: req(1)},
		"ELSEIF":     {Parse: ELSEIF, Type: Conditional, ParamRange: req(1)},
		"ELSEIFE":    {Parse: ELSEIF, Type: Conditional, ParamRange: req(1)},
		"ELSEIFB":    {Parse: ELSEIFB, Type: Conditional, ParamRange: req(1)},
		"ELSEIFNB":   {Parse: ELSEIFB, Type: Conditional, ParamRange: req(1)},
		"ELSEIFIDN":  {Parse: ELSEIFIDN, Type: Conditional, ParamRange: req(2)},
		"ELSEIFIDNI": {Parse: ELSEIFIDN, Type: Conditional, ParamRange: req(2)},
		"ELSEIFDIF":  {Parse: ELSEIFIDN, Type: Conditional, ParamRange: req(2)},
		"ELSEIFDIFI": {Parse: ELSEIFIDN, Type: Conditional, ParamRange: req(2)},
		"ELSE":       {Parse: ELSE, Type: Conditional, ParamRange: req(0)},
		"ENDIF":      {Parse: ENDIF, Type: Conditional, ParamRange: req(0)},
		"OPTION":     {Parse: OPTION, ParamRange: Range{1, -1}},
		// Macros
		"MACRO":  {Parse: MACRO, Sym: Mandatory, Type: Macro, ParamRange: Range{0, -1}},
		"FOR":    {Parse: DummyMacro, Type: Macro, ParamRange: req(2)},
		"FORC":   {Parse: DummyMacro, Type: Macro, ParamRange: Range{1, -1}}, // see JWasm's FORC.ASM
		"REPT":   {Parse: DummyMacro, Type: Macro, ParamRange: req(1)},
		"REPEAT": {Parse: DummyMacro, Type: Macro, ParamRange: req(1)},
		"WHILE":  {Parse: DummyMacro, Type: Macro, ParamRange: req(1)},
		"IRP":    {Parse: DummyMacro, Type: Macro, ParamRange: req(2)},
		"IRPC":   {Parse: DummyMacro, Type: Macro, ParamRange: req(2)},
		"ENDM":   {Parse: ENDM, Type: Macro, ParamRange: req(0)},
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
		"SEGMENT": {Parse: SEGMENT, Sym: Mandatory, Type: CodeBlock, ParamRange: Range{0, 1}},
		"ENDS":    {Parse: ENDS, Sym: Optional, ParamRange: req(0)},
		"GROUP":   {Sym: Mandatory, ParamRange: req(1)}, // TODO
		// Data allocations
		"DB": data,
		"DW": data,
		"DD": data,
		"DQ": data,
		"DF": data,
		"DP": data,
		"DT": data,
		// Structures
		"STRUCT": {Parse: STRUC, Sym: Optional, ParamRange: Range{0, 2}}, // Yes, it's possible to have
		"STRUC":  {Parse: STRUC, Sym: Optional, ParamRange: Range{0, 2}}, // unnamed structures and
		"UNION":  {Parse: STRUC, Sym: Optional, ParamRange: Range{0, 2}}, // unions inside named ones.
		// String functions (all TODO)
		"CATSTR":  {Sym: Mandatory, ParamRange: Range{1, -1}},
		"SIZESTR": {Sym: Mandatory, ParamRange: req(1)},
		"INSTR":   {Sym: Mandatory, ParamRange: Range{2, 3}},
		"SUBSTR":  {Sym: Mandatory, ParamRange: Range{2, 3}},
	}
}
