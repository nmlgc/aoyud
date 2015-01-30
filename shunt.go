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

type shuntVal interface {
	calc(retStack *shuntStack) shuntVal
	fmt.Stringer
}

func (v asmInt) calc(retStack *shuntStack) shuntVal {
	return v
}

type shuntOp struct {
	id         shuntOpID
	precedence int
	args       int
	// Function to apply to the two operands.
	// a will be pushed back onto the stack.
	function func(a, b *asmInt)
}

func (op *shuntOp) calc(retStack *shuntStack) shuntVal {
	var args [2]asmInt
	for i := 0; i < op.args; i++ {
		arg := retStack.pop()
		if arg == nil {
			return arg
		}
		args[1-i] = arg.(asmInt)
	}
	op.function(&args[0], &args[1])
	return args[0]
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

func (stack *shuntStack) pop() shuntVal {
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
func (p *parser) nextShuntToken(s *lexStream, opSet *shuntOpMap) (asmVal, error) {
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

// pushOp evaluates newOp, a newly incoming operator, in relation to the
// previous operators on top of opStack, and returns the next set of allowed
// operators.
func (retStack *shuntStack) pushOp(opStack *shuntStack, newOp *shuntOp) *shuntOpMap {
	switch newOp.id {
	case opParenR:
		top := opStack.pop()
		for top != nil && top.(*shuntOp).id != opParenL {
			retStack.push(top)
			top = opStack.pop()
		}
		if top == nil {
			log.Printf("mismatched parentheses\n")
		}
		return &binaryOperators
	case opParenL:
		opStack.push(newOp)
	default:
		for top := opStack.peek(); top != nil; top = opStack.peek() {
			op := top.(*shuntOp)
			if op.id == opParenL || newOp.precedence <= op.precedence {
				break
			}
			retStack.push(opStack.pop())
		}
		opStack.push(newOp)
	}
	return &unaryOperators
}

type shuntState struct {
	retStack shuntStack
	opStack  shuntStack
	opSet    *shuntOpMap
}

func (p *parser) shuntLoop(s *shuntState, expr string) error {
	var err error = nil
	var token asmVal
	stream := newLexStream(expr)
	for stream.peek() != eof && err == nil {
		token, err = p.nextShuntToken(stream, s.opSet)
		if err != nil {
			return err
		}
		switch token.(type) {
		case asmInt:
			s.retStack.push(token.(asmInt))
			s.opSet = &binaryOperators
		case *shuntOp:
			s.opSet = s.retStack.pushOp(&s.opStack, token.(*shuntOp))
		case asmString:
			err = p.shuntLoop(s, string(token.(asmString)))
		default:
			err = fmt.Errorf("unknown value: %s", token)
		}
		stream.ignore(&whitespace)
	}
	return err
}

// shunt converts the arithmetic expression in expr into an RPN stack.
func (p *parser) shunt(expr string) *shuntStack {
	s := &shuntState{opSet: &unaryOperators}
	if err := p.shuntLoop(s, expr); err != nil {
		log.Println(err)
		return nil
	}
	for top := s.opStack.peek(); top != nil; top = s.opStack.peek() {
		s.opStack.pop()
		if top.(*shuntOp).id == opParenL {
			log.Printf("missing a right parenthesis\n")
		} else {
			s.retStack.push(top)
		}
	}
	return &s.retStack
}

// solve evaluates the RPN stack s and returns the result.
func (s shuntStack) solve() *asmInt {
	retStack := make(shuntStack, 0, cap(s))
	for _, val := range s {
		if result := val.calc(&retStack); result != nil {
			retStack.push(result)
		}
	}
	if len(retStack) != 1 {
		log.Println("invalid RPN expression:", s)
		return nil
	}
	result := retStack[0].(asmInt)
	return &result
}

// evalBool wraps shunt and solve, and casts the result to a bool.
func (p *parser) evalBool(expr string) bool {
	if rpnStack := p.shunt(expr); rpnStack != nil {
		if ret := rpnStack.solve(); ret != nil {
			return ret.n != 0
		}
	}
	// Default to false in the case of an error... for now, at least.
	return false
}
