// Custom error type storing a list of error strings.

package main

import "fmt"

type ErrorList struct {
	s []string
}

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
		e.s = append(e.s, err.s...)
	}
	return e
}

// AddF appends a formatted error to e, and returns e itself.
func (e *ErrorList) AddF(format string, a ...interface{}) *ErrorList {
	e = e.createIfNecessary()
	e.s = append(e.s, fmt.Sprintf(format, a...))
	return e
}

// NewErrorList creates a new error list from the given existing error.
func NewErrorList(err error) *ErrorList {
	return &ErrorList{[]string{err.Error()}}
}

// ErrorListF creates a new error list from the given format string.
func ErrorListF(format string, a ...interface{}) *ErrorList {
	return &ErrorList{[]string{fmt.Sprintf(format, a...)}}
}
