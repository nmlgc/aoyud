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
	"strconv"
	"strings"
)

// DataUnit represents an emittable data type.
type DataUnit interface {
	Name() string
	Width() uint
}

type SimpleData uint

func (d SimpleData) Name() string {
	return strconv.Itoa(int(d))
}

func (d SimpleData) Width() uint {
	return uint(d)
}

// EmissionTarget represents a container that can hold data declarations, i.e.
// a segment or structure.
type EmissionTarget interface {
	Name() string
	// Offset returns the chunk and offset at the end of the emission target's
	// data block.
	Offset() (chunk uint, off uint64)
	// AddPointer adds the given pointer to the global symbol table (if the
	// symbol is supposed to be public) or the type's own one (if it has one).
	AddPointer(p *parser, sym string, ptr asmDataPtr) (err ErrorList)
	// AddData appends the given data to the end of the emission target's data
	// block. ptr can be nil if no pointer is to be emitted for data.
	AddData(ptr *asmPtr, data Emittable) (err ErrorList)
	// WordSize returns the maximum number of bytes allowed for addresses.
	WordSize() uint8
}

// Blob couples an Emittable with all the pointers that point to it.
type Blob struct {
	Ptrs []asmPtr
	Data *Emittable
}

// BlobList lists all Blobs of a single data chunk by storing a Blob with the
// same Data (but not the same Ptrs) for every byte it occupies. This allows
// easy random access of each byte within a chunk while also simplifying access
// to neighboring Blobs.
type BlobList []Blob

func (l BlobList) Append(ptr *asmPtr, data Emittable) BlobList {
	datalen := data.Len()
	if datalen > 0 {
		first := Blob{Data: &data}
		if ptr != nil {
			first.Ptrs = append(first.Ptrs, *ptr)
		}
		l = append(l, first)
		remaining := Blob{Data: &data}
		for i := uint(1); i < datalen; i++ {
			l = append(l, remaining)
		}
	}
	return l
}

// PaddedData either returns a DataArray consisting of data enlarged to
// newlen using null bytes, or data itself if it's already large enough.
func PaddedData(data Emittable, newlen uint) Emittable {
	oldlen := data.Len()
	if newlen > oldlen {
		padlen := int(newlen - oldlen)
		paddata := asmString(strings.Repeat("\x00", padlen))
		return DataArray{data, paddata}
	}
	return data
}

// Set overwrites the data of the blob at the given offset, and all identical
// prior or following blob pointers, with the given one. The third return
// value is the offset of the first element in the blob list that matches the
// data at the given offset.
func (l BlobList) Set(offset uint, data Emittable) (BlobList, ErrorList, uint) {
	if offset >= uint(len(l)) {
		return nil, ErrorListF(ESError, "value outside data block: %s", data), 0
	}
	target := l[offset].Data
	for i := offset - 1; i < offset; i-- {
		if l[i].Data != target {
			break
		}
		offset = i
	}
	first := l[offset]
	datalen := data.Len()
	targetlen := (*first.Data).Len()
	if datalen > targetlen {
		return l, ErrorListF(ESError, "value too large: %s", data), 0
	}
	newdata := PaddedData(data, targetlen)
	for i := uint(0); i < targetlen; i++ {
		l[offset+i].Data = &newdata
	}
	return l, nil, offset
}

// Expand resizes the blob at the given offset to newlen by concatenating null
// bytes to the existing blob, and adds ptr to the blob at i.
func (l BlobList) Expand(ptr *asmPtr, offset uint, newlen uint) BlobList {
	if offset < uint(len(l)) {
		olddata := *l[offset].Data
		oldlen := olddata.Len()
		if newlen > oldlen {
			newdata := PaddedData(olddata, newlen)
			newblob := Blob{Data: &newdata}
			newstart, newend := offset+oldlen, offset+newlen

			for i := offset; i < newstart; i++ {
				l[i].Data = newblob.Data
			}
			var newblobs []Blob
			for i := newstart; i < newend; i++ {
				newblobs = append(newblobs, newblob)
			}
			l = append(l[:newstart], append(newblobs, l[newstart:]...)...)
		}
		if ptr != nil {
			l[offset].Ptrs = append(l[offset].Ptrs, *ptr)
		}
	}
	return l
}

func (l BlobList) Emit() (ret []byte) {
	var last *Emittable = nil
	for _, cur := range l {
		if cur.Data != last {
			ret = append(ret, (*cur.Data).Emit()...)
			last = cur.Data
		}
	}
	return ret
}

// Dump pretty-prints the offsets, pointer names, and binary data of all blobs
// in l, indented with the given number of tabs, and also recurses into
// structure blobs.
func (l BlobList) Dump(indent int) (ret string) {
	offsetDigits := 0
	for listlen := len(l); listlen > 0; listlen /= 16 {
		offsetDigits++
	}
	longestSym := 0
	for _, blob := range l {
		for _, ptr := range blob.Ptrs {
			if ptr.sym != nil && len(*ptr.sym) > longestSym {
				longestSym = len(*ptr.sym)
			}
		}
	}

	indentStr := strings.Repeat("\t", indent)
	offsetFmt := "%s• 0%0*xh | "
	offsetPad := "\n%s   %*s  | "
	offsetPad = fmt.Sprintf(offsetPad, indentStr, offsetDigits, "")
	printSym := func(sym *string) string {
		if sym != nil {
			return fmt.Sprintf("%*s | ", longestSym, *sym)
		}
		return fmt.Sprintf("%*s | ", longestSym, " ")
	}

	var last *Emittable = nil
	for b, blob := range l {
		if blob.Data != last {
			if b > 0 {
				ret += "\n"
			}
			ret += fmt.Sprintf(offsetFmt, indentStr, offsetDigits, b)
			if len(blob.Ptrs) > 0 {
				for i, ptr := range blob.Ptrs {
					if i > 0 {
						ret += offsetPad
					}
					ret += printSym(ptr.sym)
				}
			} else {
				ret += printSym(nil)
			}
			ret += fmt.Sprintf("% x", (*blob.Data).Emit())

			switch (*blob.Data).(type) {
			case *asmStruc:
				ret += "\n" + (*blob.Data).(*asmStruc).data.Dump(indent+1)
			}
			last = blob.Data
		}
	}
	return ret
}

func (l BlobList) String() (ret string) {
	return l.Dump(0)
}

type asmPtr struct {
	sym  *string // necessary for reverse lookup
	unit DataUnit
}

// asmDataPtr represents a pointer to data in a specific segment or structure.
type asmDataPtr struct {
	ptr   asmPtr
	et    EmissionTarget
	chunk uint
	off   uint64
}

func (p asmDataPtr) Thing() string {
	return "data pointer"
}

func (p asmDataPtr) String() string {
	var offChars int = int(p.et.WordSize() * 2)
	return fmt.Sprintf("(%s*) %s:%d:%0*xh",
		p.ptr.unit.Name(), p.et.Name(), p.chunk, offChars, p.off,
	)
}

func (p asmDataPtr) Width() uint {
	return p.ptr.unit.Width()
}

type asmGroup struct {
	name string
	segs []*asmSegment
}

func (g asmGroup) Thing() string {
	return "group"
}

func (g asmGroup) String() string {
	ret := "GROUP ["
	for i, seg := range g.segs {
		if i > 0 {
			ret += ", "
		}
		ret += (*seg).Name()
	}
	return ret + "]"
}

func (g *asmGroup) Add(seg *asmSegment) (err ErrorList) {
	if seg.group != nil && seg.group != g {
		return err.AddF(ESError,
			"segment already part of group %s: %s", seg.group.name, seg.Name(),
		)
	}
	if seg.group == nil {
		seg.group = g
		g.segs = append(g.segs, seg)
	}
	return err
}

type asmSegment struct {
	name       string
	chunks     []BlobList // List of all contiguous data blocks
	group      *asmGroup
	overflowed bool
	wordsize   uint8
}

func (s asmSegment) Thing() string      { return "segment name" }
func (s asmSegment) OpenThing() string  { return "open segment" }
func (s asmSegment) OpenThings() string { return "open segments" }
func (s asmSegment) Name() string       { return s.name }
func (s asmSegment) WordSize() uint8    { return s.wordsize }

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

func (s *asmSegment) AddData(ptr *asmPtr, data Emittable) (err ErrorList) {
	maxSize := uint64((1 << (s.wordsize * 8)) - 1)
	if uint64(data.Len()+s.width()) > maxSize && !s.overflowed {
		s.overflowed = true
		err = err.AddF(ESError,
			"declaration overflows %d-bit segment: %s", s.wordsize*8, s.Name(),
		)
	}
	if len(s.chunks) == 0 {
		s.chunks = make([]BlobList, 1)
	}
	chunk := len(s.chunks) - 1
	s.chunks[chunk] = s.chunks[chunk].Append(ptr, data)
	return err
}

func (s *asmSegment) Offset() (chunk uint, off uint64) {
	if len(s.chunks) != 0 {
		chunk = uint(len(s.chunks) - 1)
		off = uint64(len(s.chunks[chunk]))
	}
	return chunk, off
}

func (s *asmSegment) AddPointer(p *parser, sym string, ptr asmDataPtr) (err ErrorList) {
	return p.syms.Set(sym, ptr, true)
}

func (p *parser) CurrentEmissionTarget() EmissionTarget {
	// It is possible to open structures inside segments, but not vice versa.
	if len(p.strucs) >= 1 {
		return p.strucs[len(p.strucs)-1].(*asmStruc)
	} else if len(p.segs) >= 1 {
		return p.segs[len(p.segs)-1].(*asmSegment)
	}
	return nil
}

func (p *parser) EmitPointer(sym string, unit DataUnit) (err ErrorList) {
	if sym == "" {
		return err
	}
	et := p.CurrentEmissionTarget()
	chunk, off := et.Offset()
	ptr := asmDataPtr{ptr: asmPtr{sym: &sym, unit: unit}, et: et, chunk: chunk}
	if p.pass2 {
		ptr.off = off
	}
	return et.AddPointer(p, sym, ptr)
}

func (p *parser) EmitData(it *item, unit DataUnit) (err ErrorList) {
	err = p.EmitPointer(it.sym, unit)

	// In structures, we need to emit data even in pass 1 in order to have
	// their size at the beginning of pass 2. In segments, we don't; in fact,
	// doing so effectively emits all data twice, with all pointers pointing to
	// the second, unnecessary copy.
	if p.pass2 || len(p.strucs) > 0 {
		blob, errData := p.syms.evalData(it.pos, it.params[0], unit)
		err = err.AddL(errData)
		if errData.Severity() < ESError {
			ptr := &asmPtr{sym: &it.sym, unit: unit}
			err = err.AddL(p.CurrentEmissionTarget().AddData(ptr, blob))
		}
	}
	return err
}

func (p *parser) AddToDGroup(seg *asmSegment) (err ErrorList) {
	if p.intSyms.Model != nil && *p.intSyms.Model&Flat == 0 {
		dgroup, err := p.syms.GetGroup("DGROUP")
		return err.AddL(dgroup.Add(seg))
	}
	return nil
}
