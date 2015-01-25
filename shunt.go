// Shunting-yard parsing of arithmetic expressions.

package main

import (
	"fmt"
	"log"
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
)

type shuntVal asmVal
type shuntOp struct {
	id         shuntOpID
	precedence int
	args       int
	// Function to apply to the two operands.
	// a will be pushed back onto the stack.
	function func(a, b *asmInt)
}

func (s *shuntOp) String() string {
	return string(s.id)
}

type shuntOpMap map[string]shuntOp
type shuntOpStack []*shuntOp
type shuntValStack []asmInt

// "Just don't bother with making this generic," they said.
// "That's slow and error-prone and not type-safe at all," they said.
func (stack *shuntOpStack) push(element *shuntOp) {
	*stack = append(*stack, element)
}

func (stack *shuntOpStack) peek() *shuntOp {
	if length := len(*stack); length != 0 {
		return (*stack)[length-1]
	}
	return nil
}

func (stack *shuntOpStack) pop() *shuntOp {
	if ret := stack.peek(); ret != nil {
		*stack = (*stack)[:len(*stack)-1]
		return ret
	}
	log.Println("arithmetic stack underflow")
	return nil
}

func (stack *shuntValStack) push(element asmInt) {
	*stack = append(*stack, element)
}

func (stack *shuntValStack) peek() *asmInt {
	if length := len(*stack); length != 0 {
		return &(*stack)[length-1]
	}
	return nil
}

func (stack *shuntValStack) pop() *asmInt {
	if ret := stack.peek(); ret != nil {
		*stack = (*stack)[:len(*stack)-1]
		return ret
	}
	log.Println("arithmetic stack underflow")
	return nil
}

// Why, Go, why.
func b2i(b bool) int64 {
	if b {
		return 1
	}
	return 0
}

var unaryOperators = shuntOpMap{
	"(":   {opParenL, 14, 0, nil},
	")":   {opParenR, 14, 0, nil},
	"+":   {opPlus, 8, 1, func(a, b *asmInt) {}},
	"-":   {opMinus, 8, 1, func(a, b *asmInt) { a.n = -b.n }},
	"NOT": {opNot, 4, 1, func(a, b *asmInt) { a.n = ^b.n }},
}

var binaryOperators = shuntOpMap{
	"(":   {opParenL, 14, 0, nil},
	")":   {opParenR, 14, 0, nil},
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
func (p *parser) nextShuntToken(s *lexStream, opSet *shuntOpMap) (shuntVal, error) {
	token := s.nextToken(&shuntDelim)
	if isAsmInt(token) {
		return newAsmInt(token)
	}
	tokenUpper := strings.ToUpper(token)
	if nextOp, ok := (*opSet)[tokenUpper]; ok {
		return &nextOp, nil
	}
	return p.getSym(p.toSymCase(token))
}

// perform applies the function of the given operator on the top of valStack.
func (valStack *shuntValStack) performOp(op *shuntOp) bool {
	var args [2]asmInt
	for i := 0; i < op.args; i++ {
		arg := valStack.pop()
		if arg == nil {
			return false
		}
		args[1-i] = *arg
	}
	op.function(&args[0], &args[1])
	valStack.push(args[0])
	return true
}

// evalOp evaluates newOp, a newly incoming operator, in relation to the
// previous operators on top of opStack, and returns the next set of allowed
// operators.
func (valStack *shuntValStack) evalOp(opStack *shuntOpStack, newOp *shuntOp) *shuntOpMap {
	switch newOp.id {
	case opParenR:
		top := opStack.pop()
		for top != nil && top.id != opParenL {
			valStack.performOp(top)
			top = opStack.pop()
		}
		if top == nil {
			log.Printf("mismatched parentheses\n")
		}
		return &binaryOperators
	case opParenL:
		opStack.push(newOp)
	default:
		top := opStack.peek()
		for top != nil && top.id != opParenL && newOp.precedence <= top.precedence {
			opStack.pop()
			valStack.performOp(top)
			top = opStack.peek()
		}
		opStack.push(newOp)
	}
	return &unaryOperators
}

type shuntState struct {
	valStack shuntValStack
	opStack  shuntOpStack
	opSet    *shuntOpMap
}

func (p *parser) shuntLoop(s *shuntState, expr string) error {
	var err error = nil
	var token shuntVal
	stream := newLexStream(expr)
	for stream.peek() != eof && err == nil {
		token, err = p.nextShuntToken(stream, s.opSet)
		if err != nil {
			return err
		}
		switch token.(type) {
		case asmInt:
			s.valStack.push(token.(asmInt))
			s.opSet = &binaryOperators
		case *shuntOp:
			s.opSet = s.valStack.evalOp(&s.opStack, token.(*shuntOp))
		case asmString:
			err = p.shuntLoop(s, string(token.(asmString)))
		default:
			err = fmt.Errorf("unknown value: %s", token)
		}
		stream.ignore(&whitespace)
	}
	return err
}

// shunt performs the arithmetic expression in expr and returns the result.
func (p *parser) shunt(expr string) (asmInt, error) {
	s := &shuntState{opSet: &unaryOperators}
	if err := p.shuntLoop(s, expr); err != nil {
		return asmInt{}, err
	}
	for top := s.opStack.peek(); top != nil; top = s.opStack.peek() {
		s.opStack.pop()
		if top.id == opParenL {
			log.Printf("missing a right parenthesis\n")
		} else {
			s.valStack.performOp(top)
		}
	}
	if len(s.valStack) != 1 {
		return asmInt{}, fmt.Errorf("invalid arithmetic expression: %s", expr)
	}
	return s.valStack[0], nil
}

// evalBool wraps shunt, displays its error message, and casts its result to a
// bool.
func (p *parser) evalBool(expr string) bool {
	ret, err := p.shunt(expr)
	// Default to false in the case of an error... for now, at least.
	if err != nil {
		log.Println(err)
		return false
	}
	return ret.n != 0
}
