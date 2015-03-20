package main

import (
	"log"
)

// lexStream provides methods to iteratively read through a byte stream using
// delimiter characters.
type lexStream struct {
	input string
	pos   int
}

const eof = 0

// ignore consumes bytes from the input until they stop matching the given
// character group.
func (s *lexStream) ignore(delim *charGroup) {
	for delim.matches(s.peek()) {
		s.next()
	}
}

// peek returns but does not consume the next byte in the input.
func (s *lexStream) peek() byte {
	if s.pos >= len(s.input) {
		return eof
	}
	return s.input[s.pos]
}

// next consumes the next byte in the input.
func (s *lexStream) next() byte {
	ret := s.peek()
	s.pos++
	return ret
}

// nextAssert consumes the next byte in the input and prints an error message
// if is not equal to b.
func (s *lexStream) nextAssert(b byte, prev string) bool {
	ret := s.next() == b
	if !ret {
		log.Printf("missing a closing %c: %s", b, prev)
	}
	return ret
}

// peekUntil returns but does not consume the next word that is delimited by
// the given character group.
func (s *lexStream) peekUntil(delim *charGroup) string {
	pos := s.pos
	ret := s.nextUntil(delim)
	s.pos = pos
	return ret
}

// nextUntil consumes the next word that is delimited by the given character group.
func (s *lexStream) nextUntil(delim *charGroup) string {
	s.ignore(&whitespace)
	start := s.pos
	for !delim.matches(s.peek()) && s.peek() != eof {
		s.next()
	}
	return s.input[start:s.pos]
}

// nextToken works like nextUntil, but consumes one additional character if
// the returned string would have been empty.
func (s *lexStream) nextToken(delim *charGroup) string {
	ret := s.nextUntil(delim)
	if len(ret) == 0 {
		ret = string(s.next())
	}
	return ret
}

// nextSegmentParam returns the next token delimited by either whitespace
// or quotes.
func (s *lexStream) nextSegmentParam() string {
	ret := s.nextUntil(&segmentDelim)
	if next := s.peek(); len(ret) == 0 && quotes.matches(next) {
		nextStr := string(s.next())
		ret = nextStr + s.nextUntil(&charGroup{next})
		s.nextAssert(next, ret)
		ret += nextStr
	}
	return ret
}

// nextParam consumes and returns the next parameter to an instruction, taking
// nesting into account.
func (s *lexStream) nextParam() string {
	var quote byte
	level := 0

	s.ignore(&whitespace)
	start := s.pos
	for !(level == 0 && paramDelim.matches(s.peek())) && s.peek() != eof {
		b := s.next()

		if level == 0 && b == '\\' {
			s.nextUntil(&linebreak)
			s.ignore(&linebreak)
		}
		var leavecond bool
		ll := nestLevelLeave[b]
		if quote != 0 {
			leavecond = (b == quote)
		} else {
			leavecond = (level & ll) != 0
		}
		if leavecond {
			level &= ^ll
			quote = 0
		} else if le := nestLevelEnter[b]; le > level {
			level |= le
			if b == '\'' || b == '"' {
				quote = b
			}
		}
	}
	for s.pos > start && whitespace.matches(s.input[s.pos-1]) {
		s.pos--
	}
	return s.input[start:s.pos]
}

func newLexStream(input string) *lexStream {
	return &lexStream{input: input}
}
