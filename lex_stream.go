package main

// lexStream provides methods to iteratively read through a byte stream using
// delimiter characters.
type lexStream struct {
	input []byte
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

// peekUntil returns but does not consume the next word that is delimited by
// the given character group.
func (s *lexStream) peekUntil(delim *charGroup) []byte {
	pos := s.pos
	ret := s.nextUntil(delim)
	s.pos = pos
	return ret
}

// nextUntil consumes the next word that is delimited by the given character group.
func (s *lexStream) nextUntil(delim *charGroup) []byte {
	s.ignore(&whitespace)
	start := s.pos
	for !delim.matches(s.peek()) && s.peek() != eof {
		s.next()
	}
	return s.input[start:s.pos]
}

// nextParam consumes and returns the next parameter to an instruction, taking
// nesting into account.
func (s *lexStream) nextParam() []byte {
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

func newLexStream(input []byte) *lexStream {
	return &lexStream{input: input}
}
