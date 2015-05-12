package main

type charGroup []byte

var linebreak = charGroup{'\r', '\n'}
var whitespace = charGroup{' ', '\t'}
var quotes = charGroup{'\'', '"'}
var paramDelim = append(charGroup{',', ';'}, linebreak...)
var insDelim = append(append(charGroup{':', '='}, whitespace...), paramDelim...)
var shuntDelim = append(charGroup{
	'+', '-', '*', '/', '|', '(', ')', '[', ']', '<', '>', ':', '&', '"', '\'', ',',
}, whitespace...)
var macroDelim = append(charGroup{','}, shuntDelim...)
var segmentDelim = append(charGroup{'\'', '"'}, whitespace...)

func (g charGroup) matches(b byte) bool {
	for _, v := range g {
		if v == b {
			return true
		}
	}
	return false
}

// lexStream provides methods to iteratively read through a byte stream using
// delimiter characters.
type lexStream struct {
	input string
	c     int // Current character within the input string
	pos   ItemPos
}

const eof = 0

// ignore consumes bytes from the input until they stop matching the given
// character group.
func (s *lexStream) ignore(delim charGroup) {
	for delim.matches(s.peek()) {
		s.next()
	}
}

// peek returns but does not consume the next byte in the input.
func (s *lexStream) peek() byte {
	if s.c >= len(s.input) {
		s.pos[len(s.pos)-1].line = 0
		return eof
	}
	return s.input[s.c]
}

// next consumes the next byte in the input.
func (s *lexStream) next() byte {
	ret := s.peek()
	s.c++
	if ret == '\n' {
		s.pos[len(s.pos)-1].line++
	}
	return ret
}

// nextAssert consumes the next byte in the input and returns a warning if it
// is not equal to b.
func (s *lexStream) nextAssert(b byte, prev string) ErrorList {
	if ret := s.next() == b; !ret {
		return ErrorListF(ESWarning, "missing a closing %c: %s", b, prev)
	}
	return nil
}

// peekUntil returns but does not consume the next word that is delimited by
// the given character group.
func (s *lexStream) peekUntil(delim charGroup) string {
	tmp := *s
	return tmp.nextUntil(delim)
}

// nextUntil consumes the next word that is delimited by the given character group.
func (s *lexStream) nextUntil(delim charGroup) string {
	if s.peek() == eof {
		return ""
	}
	s.ignore(whitespace)
	start := s.c
	for !delim.matches(s.peek()) && s.peek() != eof {
		s.next()
	}
	return s.input[start:s.c]
}

// nextToken works like nextUntil, but consumes one additional character if
// the returned string would have been empty.
func (s *lexStream) nextToken(delim charGroup) string {
	ret := s.nextUntil(delim)
	if len(ret) == 0 {
		ret = string(s.next())
	}
	return ret
}

// nextSegmentParam returns the next token delimited by either whitespace
// or quotes.
func (s *lexStream) nextSegmentParam() (ret string, err ErrorList) {
	ret = s.nextUntil(segmentDelim)
	if next := s.peek(); len(ret) == 0 && quotes.matches(next) {
		nextStr := string(s.next())
		ret = nextStr + s.nextUntil(charGroup{next})
		err = s.nextAssert(next, ret)
		ret += nextStr
	}
	return ret, err
}

// nextParam consumes and returns the next parameter to an instruction, taking
// nesting into account.
func (s *lexStream) nextParam() string {

	// nestChars maps the start delimiter of the various nesting levels used
	// in MASM's syntax to their respective end delimiters.
	var nestChars = map[byte]byte{
		'{':  '}',
		'(':  ')',
		'<':  '>',
		'"':  '"',
		'\'': '\'',
	}

	type nestLevel struct {
		delim byte
		prev  *nestLevel
	}

	var quote byte
	var nest *nestLevel

	s.ignore(whitespace)
	start := s.c
	for !(nest == nil && paramDelim.matches(s.peek())) && s.peek() != eof {
		b := s.next()

		if nest == nil && b == '\\' {
			s.nextUntil(linebreak)
			s.ignore(linebreak)
		}
		leavecond := false
		if nest != nil {
			leavecond = (b == nest.delim)
		}
		if leavecond {
			nest = nest.prev
			quote = 0
		} else if ll := nestChars[b]; ll != 0 && quote == 0 {
			if b == '\'' || b == '"' {
				quote = b
			}
			nest = &nestLevel{delim: ll, prev: nest}
		}
	}
	for s.c > start && whitespace.matches(s.input[s.c-1]) {
		s.c--
	}
	return s.input[start:s.c]
}

// NewLexStream creates a new lex stream at the start of the given file.
func NewLexStream(filename *string, input string) *lexStream {
	return &lexStream{pos: NewItemPos(filename, 1), input: input}
}

// NewLexStreamAt creates a new lex stream at the given position.
func NewLexStreamAt(pos ItemPos, input string) *lexStream {
	var posCopy ItemPos
	posCopy = append(posCopy, pos...)
	return &lexStream{pos: posCopy, input: input}
}
