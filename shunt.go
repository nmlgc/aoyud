// Shunting-yard parsing of arithmetic expressions.

package main

import (
	"fmt"
	"strings"
)

// Eh, why not, helps debugging.
type shuntOpID string

const (
	opPlus  = "+"
	opMinus = "-"
	opMul   = "*"
	opDiv   = "/"
	opMod   = "mod"
	opShL   = "shl"
	opShR   = "shr"

	opAnd = "and"
	opOr  = "or"
	opXor = "xor"

	opEq = "eq"
	opNe = "ne"
	opLt = "lt"
	opLe = "le"
	opGt = "gt"
	opGe = "ge"

	opNot = "not"

	opParenL = "("
	opParenR = ")"

	opPtr = "ptr"
)

type shuntVal interface {
	calc(retStack *shuntStack) (shuntVal, *ErrorList)
	fmt.Stringer
}

func (v asmInt) calc(retStack *shuntStack) (shuntVal, *ErrorList) {
	return v, nil
}

func (v asmString) calc(retStack *shuntStack) (shuntVal, *ErrorList) {
	return v.toInt()
}

type shuntOp struct {
	id         shuntOpID
	precedence int
	args       int
	// Function to apply to the two operands.
	// a will be pushed back onto the stack.
	function func(a, b *asmInt)
}

func (op *shuntOp) Thing() string {
	return "arithmetic operator"
}

func (op *shuntOp) width() uint {
	return 0
}

func (op *shuntOp) calc(retStack *shuntStack) (shuntVal, *ErrorList) {
	var args [2]asmInt
	for i := 0; i < op.args; i++ {
		arg, err := retStack.pop()
		if err != nil {
			return arg, err
		}
		args[1-i] = arg.(asmInt)
	}
	op.function(&args[0], &args[1])
	return args[0], nil
}

func (op *shuntOp) String() string {
	return string(op.id)
}

type shuntOpMap map[string]shuntOp
type shuntStack []shuntVal

func (stack *shuntStack) push(element shuntVal) {
	*stack = append(*stack, element)
}

func (stack *shuntStack) peek() shuntVal {
	if length := len(*stack); length != 0 {
		return (*stack)[length-1]
	}
	return nil
}

func (stack *shuntStack) pop() (shuntVal, *ErrorList) {
	if ret := stack.peek(); ret != nil {
		*stack = (*stack)[:len(*stack)-1]
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
	"+":   {opPlus, 8, 1, func(a, b *asmInt) { a.base = b.base }},
	"-":   {opMinus, 8, 1, func(a, b *asmInt) { a.n = -b.n; a.base = b.base }},
	"NOT": {opNot, 4, 1, func(a, b *asmInt) { a.n = ^b.n; a.base = b.base }},
}

var binaryOperators = shuntOpMap{
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
}

// nextShuntToken returns the next operand or operator from s. Only operators
// in opSet are identified as such.
func (p *parser) nextShuntToken(s *lexStream, opSet *shuntOpMap) (asmVal, *ErrorList) {
	token := s.nextToken(&shuntDelim)
	if isAsmInt(token) {
		return newAsmInt(token)
	} else if quote := token[0]; quotes.matches(quote) && len(token) == 1 {
		token = s.nextUntil(&charGroup{quote})
		err := s.nextAssert(quote, token)
		return asmString(token), err
	}
	tokenUpper := strings.ToUpper(token)
	if typ, ok := asmTypes[tokenUpper]; ok {
		return typ, nil
	} else if nextOp, ok := (*opSet)[tokenUpper]; ok {
		return &nextOp, nil
	}
	return p.syms.Get(token)
}

// pushOp evaluates newOp, a newly incoming operator, in relation to the
// previous operators on top of opStack, and returns the next set of allowed
// operators.
func (retStack *shuntStack) pushOp(opStack *shuntStack, newOp *shuntOp) (*shuntOpMap, *ErrorList) {
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

func (p *parser) shuntLoop(s *shuntState, expr string) *ErrorList {
	var err *ErrorList
	var token asmVal
	stream := newLexStream(expr)
	for stream.peek() != eof && err == nil {
		token, err = p.nextShuntToken(stream, s.opSet)
		if err.Severity() >= ESError {
			return err
		}
		switch token.(type) {
		case asmInt:
			s.retStack.push(token.(asmInt))
			s.opSet = &binaryOperators
		case asmString:
			s.retStack.push(token.(asmString))
			s.opSet = &binaryOperators
		case *shuntOp:
			s.opSet, err = s.retStack.pushOp(&s.opStack, token.(*shuntOp))
		case asmExpression:
			err = p.shuntLoop(s, string(token.(asmExpression)))
		default:
			err = err.AddF(ESError,
				"can't use %s in arithmetic expression", token.Thing(),
			)
		}
		stream.ignore(&whitespace)
	}
	return err
}

// shunt converts the arithmetic expression in expr into an RPN stack.
func (p *parser) shunt(expr string) (*shuntStack, *ErrorList) {
	var err *ErrorList
	s := &shuntState{opSet: &unaryOperators}
	if err = p.shuntLoop(s, expr); err != nil {
		return nil, err
	}
	for top := s.opStack.peek(); top != nil; top = s.opStack.peek() {
		s.opStack.pop()
		if top.(*shuntOp).id == opParenL {
			err = err.AddF(ESError, "missing a right parenthesis")
		} else {
			s.retStack.push(top)
		}
	}
	return &s.retStack, err
}

// solve evaluates the RPN stack s and returns the result.
func (s shuntStack) solve() (*asmInt, *ErrorList) {
	var errList *ErrorList
	retStack := make(shuntStack, 0, cap(s))
	for _, val := range s {
		result, err := val.calc(&retStack)
		if err == nil {
			retStack.push(result)
		}
		errList = errList.AddL(err)
	}
	if len(retStack) != 1 {
		return nil, errList.AddF(ESError, "invalid RPN expression: %s", s)
	}
	result := retStack[0].(asmInt)
	return &result, errList
}

// evalInt wraps shunt and solve.
func (p *parser) evalInt(expr string) (*asmInt, *ErrorList) {
	rpnStack, err := p.shunt(expr)
	if err == nil {
		return rpnStack.solve()
	}
	return nil, err
}

// evalBool wraps evalInt and casts its result to a bool.
func (p *parser) evalBool(expr string) (bool, *ErrorList) {
	ret, err := p.evalInt(expr)
	if err == nil {
		return ret.n != 0, err
	}
	// Default to false in the case of an error... for now, at least.
	return false, err
}
