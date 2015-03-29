// Custom error type storing a list of error strings.

package main

import "fmt"

type Error struct {
	s   string
	pos *ItemPos // Optionally overrides the default position used for logging.
}

type ErrorList []Error

func (e *ErrorList) createIfNecessary() *ErrorList {
	if e == nil {
		e = &ErrorList{}
	}
	return e
}

// AddL appends an existing error list to e, and returns e itself.
func (e *ErrorList) AddL(err *ErrorList) *ErrorList {
	if err != nil {
		e = e.createIfNecessary()
		*e = append(*e, (*err)...)
	}
	return e
}

// AddF appends a formatted error to e, and returns e itself.
func (e *ErrorList) AddF(format string, a ...interface{}) *ErrorList {
	return e.AddFAt(nil, format, a...)
}

// AddFAt appends a formatted error at the given code position to e, and
// returns e itself.
func (e *ErrorList) AddFAt(pos *ItemPos, format string, a ...interface{}) *ErrorList {
	e = e.createIfNecessary()
	*e = append(*e, Error{s: fmt.Sprintf(format, a...), pos: pos})
	return e
}

// NewErrorList creates a new error list from the given existing error.
func NewErrorList(err error) *ErrorList {
	return &ErrorList{Error{s: err.Error()}}
}

// ErrorListF creates a new error list from the given format string.
func ErrorListF(format string, a ...interface{}) *ErrorList {
	return ErrorListFAt(nil, format, a...)
}

// ErrorListFAt creates a new error list with an error from the given format
// string at the given code position.
func ErrorListFAt(pos *ItemPos, format string, a ...interface{}) *ErrorList {
	return &ErrorList{Error{s: fmt.Sprintf(format, a...), pos: pos}}
}
