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

func ErrBadType(scan *scanner.Scanner, atype AtomType) error {
	return fmt.Errorf("expected %v at %v", atype, scan.Pos())
}

func ErrSyntax(scan *scanner.Scanner, tok rune) error {
	return fmt.Errorf("unexpected token %q at %v", string(tok), scan.Pos())
}

func ErrAt(scan *scanner.Scanner, msg string) error {
	return fmt.Errorf("%v at %v", msg, scan.Pos())
}

type AtomType int

func (t AtomType) String() string {
	return TypeString(t)
}

const (
	NilType AtomType = iota
	SymbolType
	VarType
	StringType
	NumberType
	BoolType
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
	case BoolType:
		return "BOOLEAN"
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

func (a Exec) isOp() bool {
	v := string(a)
	return len(v) == 1 && isOp(rune(v[0]))
}

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

type Bool bool

func (a Bool) Type() AtomType { return BoolType }
func (a Bool) Value() any     { return bool(a) }
func (a Bool) String() string { return strconv.FormatBool(bool(a)) }

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

func Len[T List | Tuple](l T) int {
	return len(l)
}

func Peek[T List | Tuple](l T) Atom {
	ll := len(l)

	if ll == 0 {
		return Nil{}
	}

	return l[ll-1]
}

func PeekAt[T List | Tuple](l T, i int) Atom {
	ll := len(l)

	if ll <= i {
		return Nil{}
	}

	return l[ll-i-1]
}

func At[T List | Tuple](l T, i int) Atom {
	ll := len(l)

	if i < 0 || i > ll-1 {
		return Nil{}
	}

	return l[i]
}

type Native func(stack List) (Atom, List)

type Function struct {
	name   Symbol
	args   Tuple
	code   List
	native Native
}

type Context struct {
	prog      List
	stack     List
	functions map[string]Function
	vars      map[string]Atom
}

func (c *Context) Program() List {
	return c.prog
}

func (c *Context) String() string {
	return c.prog.String()
}

func (c *Context) Run() Atom {
	for _, a := range c.prog {
		switch a.Type() {
		case ExecType:

		default:
			c.stack = Push(c.stack, a)
		}
	}

	return Peek(c.stack)
}

var (
	operators = map[string]Function{
		"+": Function{
			native: func(stack List) (Atom, List) {
				var a, b Atom
				var res float64

				a, stack = Pop(stack)
				b, stack = Pop(stack)

				if n, ok := a.(Number); ok {
					res = n.Value().(float64)
				}

				if n, ok := b.(Number); ok {
					res += n.Value().(float64)
				}

				return Number(res), stack
			},
		},
		"-": Function{
			native: func(stack List) (Atom, List) {
				var a, b Atom
				var res float64

				a, stack = Pop(stack)
				b, stack = Pop(stack)

				if n, ok := a.(Number); ok {
					res = n.Value().(float64)
				}

				if n, ok := b.(Number); ok {
					res -= n.Value().(float64)
				}

				return Number(res), stack
			},
		},
		"*":  Function{},
		"/":  Function{},
		"+=": Function{},
		"-=": Function{},
		"*=": Function{},
		"/=": Function{},
	}
)

// Math or logic operators
func isOp(tok rune) bool {
	return strings.Contains("<>!=&|+-/*%^", string(tok))
}

func parseDef[T List | Tuple](prog T, scan *scanner.Scanner) (Function, T, error) {
	if len(prog) < 2 {
		return Function{}, nil, ErrAt(scan, "not enough arguments for def")
	}

	if PeekAt(prog, 0).Type() != SymbolType {
		return Function{}, nil, ErrAt(scan, "expected symbol")
	}

	if PeekAt(prog, 1).Type() != ListType {
		return Function{}, nil, ErrAt(scan, "expected list")
	}

	var v Atom

	v, prog = Pop(prog)
	sym := v.(Symbol)

	v, prog = Pop(prog)
	lis := v.(List)

	var args Tuple

	if At(lis, 0).Type() == TupleType {
		v, lis = At(lis, 0), lis[1:]
		args = v.(Tuple)
	}

	ff := Function{name: sym, args: args, code: lis}
	return ff, prog, nil
}

func parseArray[T List | Tuple](scan *scanner.Scanner, c *Context) (Atom, error) {
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
				l, err = parseArray[List](scan, c)
			} else {
				l, err = parseArray[Tuple](scan, c)
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

			token := scan.TokenText()

			if varmark {
				a = Var(token)
				varmark = false
			} else if qmark {
				a = Symbol(token)
				qmark = false
			} else {
				// here we should actually try to execute this
				switch token {
				case "true":
					a = Bool(true)
				case "false":
					a = Bool(false)
				case "def":
					f, ll, err := parseDef(list, scan)
					if err != nil {
						return nil, err
					}
					c.functions[f.name.String()] = f
					list = ll
					continue
				default:
					a = Exec(token)
				}
			}
			list = Push(list, a)

		case scanner.String:
			list = Push(list, String(scan.TokenText()))

		case scanner.Int, scanner.Float:
			list = Push(list, ParseNumber(scan.TokenText()))

		default:
			if isOp(tok) {
				v := scan.TokenText()
				if v[0] == '=' {
					prev := Peek(list)
					if prev.Type() == ExecType && prev.(Exec).isOp() {
						_, list = Pop(list)
						v = prev.String() + v
					}
				}

				list = Push(list, Exec(v))
				continue
			}

			return nil, ErrSyntax(scan, tok)
		}
	}

	return Atom(list), nil
}

func Parse(r io.Reader) (*Context, error) {
	var scan scanner.Scanner
	var ctx Context = Context{functions: map[string]Function{}, vars: map[string]Atom{}}

	scan.Init(r)
	scan.Mode = scanner.ScanIdents | scanner.ScanInts | scanner.ScanFloats | scanner.ScanStrings

	prog, err := parseArray[List](&scan, &ctx)
	if err == nil {
		if prog.Type() == ListType {
			ctx.prog = prog.(List)
		} else {
			ctx.prog = Push(ctx.prog, prog)
		}

		return &ctx, nil
	}

	return nil, err
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

func dump(v Atom, verbose bool) {
	if verbose {
		walk(v, "")
	} else {
		fmt.Println("", v)
	}
}

func main() {
	verbose := flag.Bool("v", false, "verbose")
	peek := flag.Int("peek", -1, "if >= 0, peek value from list")
	flag.Parse()

	ctx, err := Parse(os.Stdin)
	if err != nil {
		fmt.Println("ERROR:", err)
		return
	}

	l := ctx.Program()
	if *peek >= 0 {
		fmt.Println("PEEK", *peek, ":", PeekAt(l, *peek))
	} else {
		for k, v := range ctx.functions {
			fmt.Println("function", k)
			if v.args != nil {
				fmt.Println("args:")
				fmt.Println("", v.args)
			}

			fmt.Println("code:")
			if v.native != nil {
				fmt.Println("", "<native>")
			} else {
				dump(v.code, *verbose)
			}
		}

		fmt.Println()
		fmt.Println("main")

		dump(ctx.Program(), *verbose)
	}
}
