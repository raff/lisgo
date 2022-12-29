package main

import (
	"flag"
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
	ExecType
	TupleType
	ListType
)

func TypeString(t AtomType) string {
	switch t {
	case NilType:
		return "NIL"
	case SymbolType:
		return "SYMBOL"
	case VarType:
		return "VAR"
	case StringType:
		return "STRING"
	case NumberType:
		return "NUMBER"
	case FunctionType:
		return "FUNCTION"
	case ExecType:
		return "EXEC"
	case TupleType:
		return "TUPLE"
	case ListType:
		return "LIST"
	}

	return "UNKNOWN"
}

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

type Exec string

func (a Exec) Type() AtomType { return ExecType }
func (a Exec) Value() any     { return string(a) }
func (a Exec) String() string { return string(a) }

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
func Push[T List | Tuple](l T, v Atom) T {
	return T(append(l, v))
}

func Pop[T List | Tuple](l T) (Atom, T) {
	ll := len(l)

	if ll == 0 {
		panic("nothing to pop")
	}

	v, l := l[ll-1], l[:ll-1]
	return v, l
}

// Math or logic operators
func isOp(tok rune) bool {
	return strings.Contains("<>!=&|+-/*%^", string(tok))
}

func parseArray[T List | Tuple](scan *scanner.Scanner) (Atom, error) {
	list := T{}
	isTuple := Atom(list).Type() == TupleType
	varmark := false
	qmark := false

loop:
	for tok := scan.Scan(); tok != scanner.EOF; tok = scan.Scan() {
		if tok != scanner.Ident {
			if varmark {
				return nil, ErrSyntax(scan, tok)
			} else if qmark {
				return nil, ErrSyntax(scan, tok)
			}
		}

		switch tok {
		case '[', '(':
			var l Atom
			var err error

			if tok == '[' {
				l, err = parseArray[List](scan)
			} else {
				l, err = parseArray[Tuple](scan)
			}

			if err != nil {
				return nil, err
			}

			list = Push[T](list, l)

		case ']', ')':
			if !isTuple && tok == ']' {
				break loop
			}
			if isTuple && tok == ')' {
				break loop
			}
			return nil, ErrSyntax(scan, tok)

		case '$': // use of variable marker
			varmark = true

		case '\'': // use of symbol marker
			qmark = true

		case scanner.Ident:
			var a Atom

			if varmark {
				a = Var(scan.TokenText())
				varmark = false
			} else if qmark {
				a = Symbol(scan.TokenText())
				qmark = false
			} else {
				// here we should actually try to execute this
				a = Exec(scan.TokenText())
			}
			list = Push(list, a)

		case scanner.String:
			list = Push(list, String(scan.TokenText()))

		case scanner.Int, scanner.Float:
			list = Push(list, ParseNumber(scan.TokenText()))

		default:
			if isOp(tok) {
				list = Push(list, Exec(scan.TokenText()))
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

	return parseArray[List](&scan)
}

func walk(v Atom, indent string) {
	if l, ok := v.(List); ok {
		fmt.Println(indent, "LIST")
		for _, v := range l {
			walk(v, indent+" ")
		}

		return
	}

	fmt.Println(indent, TypeString(v.Type()), v)
}

func main() {
	verbose := flag.Bool("v", false, "verbose")
	flag.Parse()

	list, err := parse(os.Stdin)
	if err != nil {
		fmt.Println("ERROR:", err)
	} else if *verbose {
		walk(list, "")
	} else {
		fmt.Println(list)
	}
}
