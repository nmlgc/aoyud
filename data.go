// Program data storage.
//
// Since we don't do actual assembling of instruction mnemonics into opcodes,
// we can't completely rebuild a program's entire data.
// However, we only actually need the correct memory representation for a
// single reason - the correct identification of array boundaries, which are
// usually only implied in assembly syntax (by giving a name to the first
// element, then emitting N more unnamed elements following it). Therefore,
// it's enough to merely store all successive data initializations into a
// single chunk of bytes, and start a new one on every non-data instruction.

package main

import (
	"fmt"
	"strings"
)

type asmDataChunk []byte

// asmDataPtr represents a pointer to data in a specific segment.
type asmDataPtr struct {
	seg *asmSegment
	off *uint64 // nil = unknown position (used during pass 1)
	w   uint
}

func (p asmDataPtr) Thing() string {
	return "data pointer"
}

func (p asmDataPtr) String() string {
	var offChars int = int(p.seg.wordsize * 2)
	var offStr string
	if p.off == nil {
		offStr = strings.Repeat("?", offChars)
	} else {
		offStr = fmt.Sprintf("%0*xh", offChars, *p.off)
	}
	return fmt.Sprintf("(%d*) %s:", p.w, p.seg.name) + offStr
}

func (p asmDataPtr) width() uint {
	return p.w
}

type asmSegment struct {
	name     string
	chunks   []asmDataChunk
	wordsize uint
	prev     *asmSegment // in order to easily handle nested segments
}

func (s asmSegment) Thing() string      { return "segment name" }
func (s asmSegment) OpenThing() string  { return "open segment" }
func (s asmSegment) OpenThings() string { return "open segments" }
func (s asmSegment) Name() string       { return s.name }

func (s asmSegment) Prev() Nestable {
	if s.prev != nil {
		return s.prev
	}
	return nil
}

func (s asmSegment) String() string {
	return fmt.Sprintf(
		"SEGMENT (%d-bit, %d bytes of data in %d chunks)",
		s.wordsize*8, s.width(), len(s.chunks),
	)
}

func (s asmSegment) width() uint {
	ret := 0
	for _, c := range s.chunks {
		ret += len(c)
	}
	return uint(ret)
}

func (s *asmSegment) Append(blob []byte) {
	if len(s.chunks) == 0 {
		s.chunks = make([]asmDataChunk, 1)
	}
	chunk := len(s.chunks) - 1
	s.chunks[chunk] = append(s.chunks[chunk], blob...)
}
