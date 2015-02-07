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
)

type asmDataChunk []byte

type asmSegment struct {
	name     string
	chunks   []asmDataChunk
	wordsize uint
	prev     *asmSegment // in order to easily handle nested segments
}

func (s asmSegment) String() string {
	return fmt.Sprintf(
		"SEGMENT (%d-bit, %d bytes of data)",
		s.wordsize*8, s.width(),
	)
}

func (s asmSegment) width() uint {
	ret := 0
	for _, c := range s.chunks {
		ret += len(c)
	}
	return uint(ret)
}
