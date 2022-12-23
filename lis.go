package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"text/scanner"
)

func ErrNoArray(atype string, scan *scanner.Scanner) error {
	return fmt.Errorf("expected %v at %v", atype, scan.Pos())
}

func ErrSyntax(scan *scanner.Scanner, tok rune) error {
	return fmt.Errorf("unexpected token %q at %v", string(tok), scan.Pos())
}

type AtomType int

const (
	NilType AtomType = iota
	SymbolType
	VarType
	StringType
	NumberType
	FunctionType
	TupleType
	ListType
)

type Atom interface {
	Type() AtomType
	Value() any
	String() string
}

type Nil struct{}

func (a Nil) Type() AtomType { return NilType }
func (a Nil) Value() any     { return nil }
func (a Nil) String() string { return "nil" }

type Symbol string

func (a Symbol) Type() AtomType { return SymbolType }
func (a Symbol) Value() any     { return string(a) }
func (a Symbol) String() string { return string(a) }

type Var string

func (a Var) Type() AtomType { return VarType }
func (a Var) Value() any     { return string(a) }
func (a Var) String() string { return "$" + string(a) }

type Number float64

func (a Number) Type() AtomType { return NumberType }
func (a Number) Value() any     { return float64(a) }
func (a Number) String() string { return strconv.FormatFloat(float64(a), 'f', -1, 64) }

func ParseNumber(s string) Number {
	n, _ := strconv.ParseFloat(s, 64)
	return Number(n)
}

type String string

func (a String) Type() AtomType { return StringType }
func (a String) Value() any     { return string(a) }
func (a String) String() string { return string(a) }

type List []Atom

func (a List) Type() AtomType { return ListType }
func (a List) Value() any     { return []Atom(a) }
func (a List) String() string { return fmt.Sprintf("%v", []Atom(a)) }

type Tuple []Atom

func (a Tuple) Type() AtomType { return TupleType }
func (a Tuple) Value() any     { return []Atom(a) }
func (a Tuple) String() string {
	s := fmt.Sprintf("%v", []Atom(a))
	return "(" + strings.Trim(s, "[]") + ")"
}

// List or Tuple
func Append[T List | Tuple](l T, v Atom) T {
	return T(append(l, v))
}

func isOp(tok rune) bool {
	return strings.Contains("<>!=&|+-/*%^", string(tok))
}

func parseArray[T List | Tuple](tok, exp rune, scan *scanner.Scanner) (Atom, error) {
	if tok != exp {
		atype := "list"
		if exp == '(' {
			atype = "tuple"
		}
		return nil, ErrNoArray(atype, scan)
	}

	list := T{}
	varmark := false

loop:
	for tok = scan.Scan(); tok != scanner.EOF; tok = scan.Scan() {
		if varmark && tok != scanner.Ident {
			return nil, ErrSyntax(scan, tok)
		}

		switch tok {
		case '[', '(':
			var l Atom
			var err error

			if tok == '[' {
				l, err = parseArray[List](tok, '[', scan)
			} else {
				l, err = parseArray[Tuple](tok, '(', scan)
			}

			if err != nil {
				return nil, err
			}

			list = Append[T](list, l)

		case ']', ')':
			if exp == '[' && tok == ']' {
				break loop
			}
			if exp == '(' && tok == ')' {
				break loop
			}
			return nil, ErrSyntax(scan, tok)

		case '$': // use of variable marker
			varmark = true

		case scanner.Ident:
			var a Atom

			if varmark {
				a = Var(scan.TokenText())
				varmark = false
			} else {
				a = Symbol(scan.TokenText())
			}
			list = Append(list, a)

		case scanner.String:
			list = Append(list, String(scan.TokenText()))

		case scanner.Int, scanner.Float:
			list = Append(list, ParseNumber(scan.TokenText()))

		default:
			if isOp(tok) {
				list = Append(list, String(scan.TokenText()))
				continue
			}

			return nil, ErrSyntax(scan, tok)
		}
	}

	return Atom(list), nil
}

func parse(r io.Reader) (Atom, error) {
	var scan scanner.Scanner

	scan.Init(r)
	scan.Mode = scanner.ScanIdents | scanner.ScanInts | scanner.ScanFloats | scanner.ScanStrings

	return parseArray[List](scan.Next(), '[', &scan)
}

func main() {
	list, err := parse(os.Stdin)
	if err != nil {
		fmt.Println("ERROR:", err)
	} else {
		fmt.Println(list)
	}
}
