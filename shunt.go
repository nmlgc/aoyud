// Shunting-yard parsing of arithmetic expressions.

package main

import (
	"bytes"
	"fmt"
	"strings"
)

// Eh, why not, helps debugging.
type OperatorID string

const (
	opPlus  = "+"
	opMinus = "-"
	opMul   = "*"
	opDiv   = "/"
	opMod   = "MOD"
	opShL   = "SHL"
	opShR   = "SHR"

	opAnd = "AND"
	opOr  = "OR"
	opXor = "XOR"

	opEq = "EQ"
	opNe = "NE"
	opLt = "LT"
	opLe = "LE"
	opGt = "GT"
	opGe = "GE"

	opNot = "NOT"

	opParenL = "("
	opParenR = ")"

	opPtr = "PTR"

	opDup    = "DUP"
	opConcat = ","
)

type shuntOp struct {
	id         OperatorID
	precedence int
	args       int
	function   interface{} // Function to apply to the operands.
}

func (op *shuntOp) Thing() string {
	return "arithmetic operator"
}

func (op *shuntOp) String() string {
	return string(op.id)
}

type shuntOpMap map[string]shuntOp

type shuntStack struct {
	vals     []Thingy
	wordsize uint
}

func (stack *shuntStack) String() string {
	return fmt.Sprintf("%v (%d-bit)", stack.vals, stack.wordsize*8)
}

func (stack *shuntStack) push(element Thingy) {
	stack.vals = append(stack.vals, element)
}

func (stack *shuntStack) peek() Thingy {
	if length := len(stack.vals); length != 0 {
		return stack.vals[length-1]
	}
	return nil
}

func (stack *shuntStack) pop() (Thingy, ErrorList) {
	if ret := stack.peek(); ret != nil {
		stack.vals = stack.vals[:len(stack.vals)-1]
		return ret, nil
	}
	return nil, ErrorListF(ESError, "arithmetic stack underflow")
}

// Why, Go, why.
func b2i(b bool) int64 {
	if b {
		return 1
	}
	return 0
}

var asmTypes = map[string]asmInt{
	"?":     {n: 0},
	"BYTE":  {n: 1},
	"WORD":  {n: 2},
	"DWORD": {n: 4},
	"PWORD": {n: 6},
	"FWORD": {n: 6},
	"QWORD": {n: 8},
	"TBYTE": {n: 10},
}

var unaryOperators = shuntOpMap{
	"(":   {opParenL, 14, 0, nil},
	")":   {opParenR, 14, 0, nil},
	"+":   {opPlus, 8, 1, func(a *asmInt) {}},
	"-":   {opMinus, 8, 1, func(a *asmInt) { a.n = -a.n }},
	"NOT": {opNot, 4, 1, func(a *asmInt) { a.n = ^a.n }},
}

var binaryOperators = shuntOpMap{
	",": {opConcat, 15, 2, nil},
	"(": {opParenL, 14, 0, nil},
	")": {opParenR, 14, 0, nil},
	"PTR": {opPtr, 11, 2, func(a, b *asmInt) {
		a.ptr = uint64(a.n)
		a.n = b.n
		a.base = b.base
	}},
	"*":   {opMul, 7, 2, func(a, b *asmInt) { a.n *= b.n }},
	"/":   {opDiv, 7, 2, func(a, b *asmInt) { a.n /= b.n }},
	"MOD": {opMod, 7, 2, func(a, b *asmInt) { a.n %= b.n }},
	"SHR": {opShR, 7, 2, func(a, b *asmInt) { a.n >>= uint(b.n) }},
	"SHL": {opShL, 7, 2, func(a, b *asmInt) { a.n <<= uint(b.n) }},
	"+":   {opPlus, 6, 2, func(a, b *asmInt) { a.n += b.n }},
	"-":   {opMinus, 6, 2, func(a, b *asmInt) { a.n -= b.n }},
	"EQ":  {opEq, 5, 2, func(a, b *asmInt) { a.n = b2i(a.n == b.n) }},
	"NE":  {opNe, 5, 2, func(a, b *asmInt) { a.n = b2i(a.n != b.n) }},
	"LT":  {opLt, 5, 2, func(a, b *asmInt) { a.n = b2i(a.n < b.n) }},
	"LE":  {opLe, 5, 2, func(a, b *asmInt) { a.n = b2i(a.n <= b.n) }},
	"GT":  {opGt, 5, 2, func(a, b *asmInt) { a.n = b2i(a.n > b.n) }},
	"GE":  {opGe, 5, 2, func(a, b *asmInt) { a.n = b2i(a.n >= b.n) }},
	"AND": {opAnd, 3, 2, func(a, b *asmInt) { a.n &= b.n }},
	"OR":  {opOr, 2, 2, func(a, b *asmInt) { a.n |= b.n }},
	"|":   {opOr, 2, 2, func(a, b *asmInt) { a.n |= b.n }},
	"XOR": {opXor, 2, 2, func(a, b *asmInt) { a.n ^= b.n }},
	"DUP": {opDup, 2, 2, nil},
}

type Emittable interface {
	Emit(width uint) []byte
}

// Since you can only go from integers to bytes, but not back, this saves us
// from having to needlessly implement Emit() for all Calcables.
type CalcToEmitOperator struct {
	Calc Calcable
}

func (cte CalcToEmitOperator) String() string {
	return cte.Calc.String()
}

func (cte CalcToEmitOperator) Emit(width uint) []byte {
	return cte.Calc.Calc().Emit(width)
}

type DUPOperator struct {
	Count Calcable
	Data  Emittable
}

func (dup DUPOperator) String() string {
	return fmt.Sprintf("(%s DUP(%s))", dup.Count, dup.Data)
}

func (dup DUPOperator) Emit(width uint) []byte {
	return bytes.Repeat(dup.Data.Emit(width), int(dup.Count.Calc().n))
}

type ConcatOperator struct {
	Data [2]Emittable
}

func (cc ConcatOperator) String() string {
	return fmt.Sprintf("(%s, %s)", cc.Data[0], cc.Data[1])
}

func (cc ConcatOperator) Emit(width uint) (ret []byte) {
	ret = append(ret, cc.Data[0].Emit(width)...)
	return append(ret, cc.Data[1].Emit(width)...)
}

type Calcable interface {
	fmt.Stringer
	Calc() asmInt
}

// No point in defining separate types for the callback functions of unary and
// binary operators, since you can't do type assertions with them anyway, for
// some bizarre reason…

type UnaryOperator struct {
	ID       OperatorID
	Function func(a *asmInt)
	Operand  Calcable
}

type BinaryOperator struct {
	ID       OperatorID
	Function func(a, b *asmInt)
	Operands [2]Calcable
}

func (v asmInt) Calc() asmInt {
	return v
}

func (op BinaryOperator) String() string {
	return fmt.Sprintf("(%s %s %s)", op.Operands[0], op.ID, op.Operands[1])
}

func (op BinaryOperator) Calc() asmInt {
	a, b := op.Operands[0].Calc(), op.Operands[1].Calc()
	op.Function(&a, &b)
	return a
}

func (op UnaryOperator) String() string {
	return fmt.Sprintf("(%s %s)", op.ID, op.Operand)
}

func (op UnaryOperator) Calc() asmInt {
	a := op.Operand.Calc()
	op.Function(&a)
	return a
}

// nextShuntToken returns the next operand or operator from s. Only operators
// in opSet are identified as such.
func (s *SymMap) nextShuntToken(stream *lexStream, opSet *shuntOpMap) (ret Thingy, err ErrorList) {
	token := stream.nextToken(shuntDelim)
	if isAsmInt(token) {
		return newAsmInt(token)
	} else if quote := token[0]; quotes.matches(quote) && len(token) == 1 {
		token = stream.nextUntil(charGroup{quote})
		err = stream.nextAssert(quote, token)
		return asmString(token), err
	}
	tokenUpper := strings.ToUpper(token)
	if typ, ok := asmTypes[tokenUpper]; ok {
		return typ, err
	} else if nextOp, ok := (*opSet)[tokenUpper]; ok {
		if nextOp.id == opDup {
			stream.ignore(whitespace)
			if stream.peek() != '(' {
				err = err.AddF(ESWarning,
					"data argument to DUP must be enclosed in parentheses",
				)
			}
		}
		return &nextOp, err
	}
	return s.Get(token)
}

// pushOp evaluates newOp, a newly incoming operator, in relation to the
// previous operators on top of opStack, and returns the next set of allowed
// operators.
func (retStack *shuntStack) pushOp(opStack *shuntStack, newOp *shuntOp) (*shuntOpMap, ErrorList) {
	switch newOp.id {
	case opParenR:
		top, err := opStack.pop()
		for top != nil && top.(*shuntOp).id != opParenL {
			retStack.push(top)
			top, err = opStack.pop()
		}
		if top == nil {
			err = err.AddF(ESError, "mismatched parentheses")
		}
		return &binaryOperators, err
	case opParenL:
		opStack.push(newOp)
	default:
		for top := opStack.peek(); top != nil; top = opStack.peek() {
			op := top.(*shuntOp)
			if op.id == opParenL || newOp.precedence <= op.precedence {
				break
			}
			retStack.push(op)
			opStack.pop()
		}
		opStack.push(newOp)
	}
	return &unaryOperators, nil
}

type shuntState struct {
	retStack shuntStack
	opStack  shuntStack
	opSet    *shuntOpMap
}

func (s *SymMap) shuntLoop(state *shuntState, pos ItemPos, expr string) (err ErrorList) {
	stream := NewLexStreamAt(pos, expr)
	for stream.peek() != eof && err.Severity() < ESError {
		token, errToken := s.nextShuntToken(stream, state.opSet)
		err = err.AddL(errToken)
		if errToken.Severity() >= ESError {
			return err
		}
		switch token.(type) {
		case asmInt:
			state.retStack.push(token)
			state.opSet = &binaryOperators
		case asmString:
			if state.retStack.wordsize > 1 {
				var errInt ErrorList
				token, errInt = token.(asmString).Int(state.retStack.wordsize)
				err = err.AddL(errInt)
			}
			state.retStack.push(token)
			state.opSet = &binaryOperators
		case *shuntOp:
			var errOp ErrorList
			state.opSet, errOp = state.retStack.pushOp(
				&state.opStack, token.(*shuntOp),
			)
			err.AddL(errOp)
		case asmExpression:
			err = err.AddL(
				s.shuntLoop(state, pos, string(token.(asmExpression))),
			)
		default:
			err = err.AddF(ESError,
				"can't use %s in arithmetic expression", token.Thing(),
			)
		}
		stream.ignore(whitespace)
	}
	return err
}

// shunt converts the arithmetic expression in expr into an RPN stack with the
// given word size.
func (s *SymMap) shunt(pos ItemPos, expr string, wordsize uint) (stack *shuntStack, err ErrorList) {
	state := &shuntState{
		opSet:    &unaryOperators,
		retStack: shuntStack{wordsize: wordsize},
	}
	if err = s.shuntLoop(state, pos, expr); err.Severity() >= ESError {
		return nil, err
	}
	for top := state.opStack.peek(); top != nil; top = state.opStack.peek() {
		state.opStack.pop()
		if top.(*shuntOp).id == opParenL {
			err = err.AddF(ESError, "missing a right parenthesis")
		} else {
			state.retStack.push(top)
		}
	}
	return &state.retStack, err
}

func (s *shuntStack) processCalcOp(op *shuntOp) (ret Calcable, err ErrorList) {
	if op.function != nil {
		if op.args == 2 {
			var err0, err1 ErrorList
			ret := BinaryOperator{
				ID: op.id, Function: op.function.(func(*asmInt, *asmInt)),
			}
			ret.Operands[1], err1 = s.ToCalcTree()
			ret.Operands[0], err0 = s.ToCalcTree()
			err = err.AddL(err1)
			err = err.AddL(err0)
			return ret, err
		} else if op.args == 1 {
			var err0 ErrorList
			ret := UnaryOperator{
				ID: op.id, Function: op.function.(func(*asmInt)),
			}
			ret.Operand, err0 = s.ToCalcTree()
			return ret, err.AddL(err0)
		}
	}
	return nil, err.AddF(ESError,
		"%s not allowed in arithmetic expression", op.String(),
	)
}

func (s *shuntStack) ToCalcTree() (Calcable, ErrorList) {
	root, err := s.pop()
	switch root.(type) {
	case nil:
		return nil, err
	case *shuntOp:
		op, errOp := s.processCalcOp(root.(*shuntOp))
		return op, err.AddL(errOp)
	case asmInt:
		return root.(asmInt), err
	case asmString:
		wordsize := s.wordsize
		if wordsize == 1 {
			wordsize = 0
		}
		integer, errInteger := root.(asmString).Int(wordsize)
		return integer, err.AddL(errInteger)
	}
	return nil, err.AddF(ESError,
		"can't use %s in arithmetic expression", root.Thing(),
	)
}

func (s *shuntStack) ToEmitTree() (Emittable, ErrorList) {
	root, err := s.pop()
	switch root.(type) {
	case nil:
		return nil, err
	case *shuntOp:
		op := root.(*shuntOp)
		switch op.id {
		case opDup:
			data, errData := s.ToEmitTree()
			count, errCount := s.ToCalcTree()
			err = err.AddL(errData)
			err = err.AddL(errCount)
			return DUPOperator{count, data}, err
		case opConcat:
			data1, err1 := s.ToEmitTree()
			data0, err0 := s.ToEmitTree()
			err = err.AddL(err1)
			err = err.AddL(err0)
			return ConcatOperator{[2]Emittable{data0, data1}}, err
		}
		cOp, errCOp := s.processCalcOp(root.(*shuntOp))
		return CalcToEmitOperator{cOp}, err.AddL(errCOp)
	case asmInt:
		return root.(asmInt), err.AddL(s.fitsInStack(root.(asmInt)))
	case asmString:
		return root.(asmString), err
	}
	return nil, err.AddF(ESError,
		"can't use %s in data expression", root.Thing(),
	)
}

// fitsInStack returns an error if v doesn't fit into the stack's word size.
func (s shuntStack) fitsInStack(v asmInt) ErrorList {
	if v.FitsIn(s.wordsize) {
		return nil
	}
	return ErrorListF(ESError, "number exceeds %d bits: %s", s.wordsize*8, v)
}

// solveInt wraps solve and enforceIntResult.
func (s shuntStack) solveInt() (*asmInt, ErrorList) {
	tree, err := s.ToCalcTree()
	if err.Severity() < ESError {
		ret := tree.Calc()
		return &ret, err.AddL(s.fitsInStack(ret))
	}
	return nil, err
}

// solveData evaluates the RPN stack s and returns the result as a blob of
// data.
func (s shuntStack) solveData() ([]byte, ErrorList) {
	tree, err := s.ToEmitTree()
	if err.Severity() < ESError {
		return tree.Emit(s.wordsize), err
	}
	return nil, err
}

// evalInt wraps shunt and solveInt.
func (s *SymMap) evalInt(pos ItemPos, expr string) (*asmInt, ErrorList) {
	rpnStack, err := s.shunt(pos, expr, maxbytes)
	if err.Severity() < ESError {
		ret, errSolve := rpnStack.solveInt()
		return ret, err.AddL(errSolve)
	}
	return nil, err
}

// evalBool wraps evalInt and casts its result to a bool.
func (s *SymMap) evalBool(pos ItemPos, expr string) (bool, ErrorList) {
	ret, err := s.evalInt(pos, expr)
	if err.Severity() < ESError {
		return ret.n != 0, err
	}
	// Default to false in the case of an error... for now, at least.
	return false, err
}

// evalData wraps shunt and solveData.
func (s *SymMap) evalData(pos ItemPos, expr string, wordsize uint) ([]byte, ErrorList) {
	rpnStack, err := s.shunt(pos, expr, wordsize)
	if err.Severity() < ESError {
		ret, errSolve := rpnStack.solveData()
		return ret, err.AddL(errSolve)
	}
	return nil, err
}
