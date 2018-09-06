// Parser and evaluator for a simple calculator with variables. The syntax is
// taken from http://eli.thegreenplace.net/2009/03/20/a-recursive-descent-parser-with-an-infix-expression-evaluator
// but simplified to have fewer levels of expressions.
//
// EBNF:
//
// <stmt>        : <assign_stmt>
//               | <if_stmt>
//               | <cmp_expr>
//
// <assign_stmt> : set <id> = <cmp_expr>
//
// Note 'else' binds to the innermost 'if', like in C. Also, this is an integer
// language and we treat 0 as "false" and anything else as "true". The
// comparison expressions evaluate to 0 or 1, accordingly.
//
// <if_stmt>     : if <cmp_expr> then <stmt> [else <stmt>]
//
// <cmp_expr>    : <arith_expr> [== <arith_expr>]
//               | <arith_expr> [!= <arith_expr>]
//               | <arith_expr> [> <arith_expr>]
//               | <arith_expr> [< <arith_expr>]
//               | <arith_expr> [>= <arith_expr>]
//               | <arith_expr> [<= <arith_expr>]
//
// <arith_expr>  : <term> {+ <term>}
//               | <term> {- <term>}
//
// <term>        : <power> {* <power>}
//               | <power> {/ <power>}
//
// <power>       : <power> ** <factor>
//               | <factor>
//
// <factor>      : <id>
//               | <number>
//               | - <factor>
//               | ( <cmp_expr> )
//
// <id>          : [a-zA-Z_]\w+
// <number>      : \d+
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"io"
	"log"
	"math"
	"strconv"
	"strings"
	"text/scanner"
)

// Operator is the enumeration type for different operators supported by our
// language.
type Operator int

const (
	ILLEGAL Operator = iota
	EQ               // ==
	NE               // !=
	GT               // >
	LT               // <
	GE               // >=
	LE               // <=
	PLUS             // +
	MINUS            // -
	MUL              // *
	DIV              // /
	POW              // **
)

func (o Operator) String() string {
	switch o {
	case ILLEGAL:
		return "<ILLEGAL>"
	case EQ:
		return "=="
	case NE:
		return "!="
	case GT:
		return ">"
	case LT:
		return "<"
	case GE:
		return ">="
	case LE:
		return "<="
	case PLUS:
		return "+"
	case MINUS:
		return "-"
	case MUL:
		return "*"
	case DIV:
		return "/"
	case POW:
		return "**"
	}
	return ""
}

type Node interface {
	isNode()
	String() string
}

type AssignStmt struct {
	Pos  scanner.Position
	Name string
	Expr Node
}

func (n AssignStmt) String() string {
	return fmt.Sprintf("AssignStmt(%s, %s)", n.Name, n.Expr)
}

func (n AssignStmt) isNode() {}

type IfStmt struct {
	Pos  scanner.Position
	Cond Node
	Then Node
	Else Node
}

func (n IfStmt) String() string {
	return fmt.Sprintf("IfStmt(%s, %s, %s)", n.Cond, n.Then, n.Else)
}

func (n IfStmt) isNode() {}

// SubExpr collects together a subexpression Node with an Operator, and is used
// in a chain of same-precedence expressions with left associativity (the
// EBNF-based solution to the natural right precedence of RD parsers). Op
// precedes Expr, and for the first Expr in the chain Op is n/a.
//
// For example, the expression "2 + 3 - 4" will be parsed into a BinaryExpr
// node with 3 children:
//
// 0: Op = n/a, Expr = 2
// 1: Op = +, Expr = 3
// 2: Op = -, Expr = 4
type SubExpr struct {
	Op   Operator
	Expr Node
}

type BinaryExpr struct {
	Pos      scanner.Position
	Children []SubExpr
}

func (n BinaryExpr) String() string {
	var b strings.Builder
	for i, c := range n.Children {
		if i != 0 {
			b.WriteString(c.Op.String())
		}
		b.WriteString(c.Expr.String())
	}
	return fmt.Sprintf("BinaryExpr(%s)", b.String())
}

func (n BinaryExpr) isNode() {}

type UnaryExpr struct {
	Pos   scanner.Position
	Child SubExpr
}

func (n UnaryExpr) String() string {
	return fmt.Sprintf("UnaryExpr(%s, %s)", n.Child.Op, n.Child.Expr)
}

func (n UnaryExpr) isNode() {}

type Identifier struct {
	Pos  scanner.Position
	Name string
}

func (n Identifier) String() string {
	return fmt.Sprintf("Identifier(%s)", n.Name)
}

func (n Identifier) isNode() {}

type IntConstant struct {
	Pos   scanner.Position
	Value int
}

func (n IntConstant) String() string {
	return fmt.Sprintf("IntConstant(%d)", n.Value)
}

func (n IntConstant) isNode() {}

type Parser struct {
	s      scanner.Scanner
	curTok rune
	curPos scanner.Position
}

// Parse takes a reader and parses a single Node from it. This is the main entry
// point ot the Parser type.
func (p *Parser) Parse(r io.Reader) (Node, error) {
	p.s.Init(r)
	p.advance(1)

	return p.parseStmt()
}

// advance consumes n tokens from the scanner, leaving he value of the last
// consumed token in curTok, also populating curPos.
func (p *Parser) advance(n int) {
	for i := 0; i < n; i++ {
		p.curTok = p.s.Scan()
		p.curPos = p.s.Position
	}
}

// curIdentIs checks whether curTok is an identifier and the identifier text
// matches s.
func (p *Parser) curIdentIs(s string) bool {
	return p.curTok == scanner.Ident && p.s.TokenText() == s
}

func (p *Parser) parseStmt() (Node, error) {
	if p.curIdentIs("if") {
		return p.parseIfStmt()
	} else if p.curIdentIs("set") {
		return p.parseAssignStmt()
	} else {
		return p.parseExpr()
	}
}

func (p *Parser) parseIfStmt() (Node, error) {
	startPos := p.curPos
	// Consume the 'if' token
	p.advance(1)
	condExpr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	// Expect and consume 'then' token
	if !p.curIdentIs("then") {
		return nil, fmt.Errorf("expected 'then' at %v", p.curPos)
	}
	p.advance(1)
	thenStmt, err := p.parseStmt()
	if err != nil {
		return nil, err
	}
	// Expect and consume 'else' token
	if !p.curIdentIs("else") {
		return nil, fmt.Errorf("expected 'else' at %v", p.curPos)
	}
	p.advance(1)
	elseStmt, err := p.parseStmt()
	if err != nil {
		return nil, err
	}
	return IfStmt{Pos: startPos, Cond: condExpr, Then: thenStmt, Else: elseStmt}, nil
}

func (p *Parser) parseAssignStmt() (Node, error) {
	startPos := p.curPos
	// Consume the 'set' token
	p.advance(1)
	id, err := p.parseIdentifier()
	if err != nil {
		return nil, err
	}
	if p.curTok != '=' {
		return nil, fmt.Errorf("expected '=' at %v", p.curPos)
	}
	// Consume the '='
	p.advance(1)
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	return AssignStmt{Pos: startPos, Name: id.(Identifier).Name, Expr: expr}, nil
}

func (p *Parser) parseIdentifier() (Node, error) {
	if p.curTok != scanner.Ident {
		return nil, fmt.Errorf("expected identifier at %v", p.curPos)
	}
	node := Identifier{Pos: p.curPos, Name: p.s.TokenText()}
	p.advance(1)
	return node, nil
}

func (p *Parser) parseExpr() (Node, error) {
	return p.parseCmpExpr()
}

// peekOperator peeks the operator at the current position. Returns ILLEGAL in
// case the operator wasn't recognized scanner is then not advanced). Note this
// will also return ILLEGAL if EOF is encountered unexpectedly.
// It does not advance the scanner; instead, the number of runes constituting
// the operator is returned as the second argument.
func (p *Parser) peekOperator() (Operator, int) {
	op := ILLEGAL
	oplen := 0
	switch p.curTok {
	case '=':
		if p.s.Peek() == '=' {
			op = EQ
			oplen = 2
		}
	case '!':
		if p.s.Peek() == '=' {
			op = NE
			oplen = 2
		}
	case '>':
		if p.s.Peek() == '=' {
			op = GE
			oplen = 2
		} else {
			op = GT
			oplen = 1
		}
	case '<':
		if p.s.Peek() == '=' {
			op = LE
			oplen = 2
		} else {
			op = LT
			oplen = 1
		}
	case '+':
		op = PLUS
		oplen = 1
	case '-':
		op = MINUS
		oplen = 1
	case '/':
		op = DIV
		oplen = 1
	case '*':
		if p.s.Peek() == '*' {
			op = POW
			oplen = 2
		} else {
			op = MUL
			oplen = 1
		}
	}
	return op, oplen
}

func (p *Parser) parseCmpExpr() (Node, error) {
	startPos := p.curPos
	lhs, err := p.parseArithExpr()
	if err != nil {
		return nil, err
	}

	op, oplen := p.peekOperator()
	if op != EQ && op != NE && op != GT && op != GE && op != LT && op != LE {
		return lhs, nil
	}
	p.advance(oplen)

	rhs, err := p.parseArithExpr()
	if err != nil {
		return nil, err
	}
	subExprs := []SubExpr{SubExpr{Op: ILLEGAL, Expr: lhs}, SubExpr{Op: op, Expr: rhs}}
	return BinaryExpr{Pos: startPos, Children: subExprs}, nil
}

func (p *Parser) parseArithExpr() (Node, error) {
	startPos := p.curPos
	lhs, err := p.parseTerm()
	if err != nil {
		return nil, err
	}
	subExprs := []SubExpr{SubExpr{Op: ILLEGAL, Expr: lhs}}

	// Loop over the other expressions as long as parsing is possible.
	for {
		op, oplen := p.peekOperator()
		if op != PLUS && op != MINUS {
			break
		}
		p.advance(oplen)

		next, err := p.parseTerm()
		if err != nil {
			return nil, err
		}
		subExprs = append(subExprs, SubExpr{Op: op, Expr: next})
	}
	return BinaryExpr{Pos: startPos, Children: subExprs}, nil
}

func (p *Parser) parseTerm() (Node, error) {
	startPos := p.curPos
	lhs, err := p.parsePower()
	if err != nil {
		return nil, err
	}
	subExprs := []SubExpr{SubExpr{Op: ILLEGAL, Expr: lhs}}
	for {
		op, oplen := p.peekOperator()
		if op != MUL && op != DIV {
			break
		}
		p.advance(oplen)

		next, err := p.parsePower()
		if err != nil {
			return nil, err
		}
		subExprs = append(subExprs, SubExpr{Op: op, Expr: next})
	}
	if len(subExprs) == 1 {
		return subExprs[0].Expr, nil
	} else {
		return BinaryExpr{Pos: startPos, Children: subExprs}, nil
	}
}

func (p *Parser) parsePower() (Node, error) {
	startPos := p.curPos
	lhs, err := p.parseFactor()
	if err != nil {
		return nil, err
	}

	// Note: power operatios are right-associative. Therefore, this method
	// recurses into rhs rather than running a loop like parseTerm or
	// parseArithExpr.
	op, oplen := p.peekOperator()
	if op != POW {
		return lhs, nil
	}
	p.advance(oplen)
	rhs, err := p.parsePower()
	if err != nil {
		return nil, err
	}
	return BinaryExpr{Pos: startPos, Children: []SubExpr{SubExpr{Op: ILLEGAL, Expr: lhs}, SubExpr{Op: POW, Expr: rhs}}}, nil
}

func (p *Parser) parseFactor() (Node, error) {
	startPos := p.curPos
	if p.curTok == '(' {
		p.advance(1)
		expr, err := p.parseCmpExpr()
		if err != nil {
			return nil, err
		}
		if p.curTok != ')' {
			return nil, fmt.Errorf("expected ')' at %v", p.curPos)
		}
		p.advance(1)
		return expr, nil
	} else if p.curTok == '-' {
		p.advance(1)
		fact, err := p.parseFactor()
		if err != nil {
			return nil, err
		}
		return UnaryExpr{Pos: startPos, Child: SubExpr{Op: MINUS, Expr: fact}}, err
	} else if p.curTok == scanner.Int {
		num, err := strconv.Atoi(p.s.TokenText())
		if err != nil {
			return nil, err
		}
		p.advance(1)
		return IntConstant{Pos: startPos, Value: num}, nil
	} else if p.curTok == scanner.Ident {
		idNode := Identifier{Pos: startPos, Name: p.s.TokenText()}
		p.advance(1)
		return idNode, nil
	} else {
		return nil, fmt.Errorf("expected factor at %v, got %s", p.curPos, p.s.TokenText())
	}
}

func ParseMultiple(ss []string) []Node {
	var nodes []Node
	for _, s := range ss {
		var p Parser
		n, err := p.Parse(strings.NewReader(s))
		if err != nil {
			log.Fatalf("Error while parsing '%s': %v", s, err)
		}
		nodes = append(nodes, n)
	}
	return nodes
}

// Evaluator is a stateful evaluator for a sequence of Nodes. After creation,
// repeatedly call Eval(n) for the next node in the session - within a session
// the Evaluator remembers what variables (defined with 'set') refer to. For
// a new/different session, just create a new Evaluator.
//
// Create new evaluators with the NewEvaluator constructor.
type Evaluator struct {
	symbolTable map[string]int
}

func NewEvaluator() *Evaluator {
	return &Evaluator{symbolTable: make(map[string]int)}
}

func (eval *Evaluator) Eval(n Node) (int, error) {
	switch nt := n.(type) {
	case IntConstant:
		return nt.Value, nil
	case Identifier:
		val, found := eval.symbolTable[nt.Name]
		if !found {
			return 0, fmt.Errorf("undefined variable '%s'", nt.Name)
		}
		return val, nil
	case UnaryExpr:
		childVal, err := eval.Eval(nt.Child.Expr)
		if err != nil {
			return 0, err
		}
		if nt.Child.Op == MINUS {
			return -childVal, nil
		} else {
			return 0, fmt.Errorf("unsupported unary operator '%s'", nt.Child.Op)
		}
	case BinaryExpr:
		var val int
		for i, se := range nt.Children {
			childVal, err := eval.Eval(se.Expr)
			if err != nil {
				return 0, err
			}
			if i == 0 {
				// For the first child, set the starting value.
				val = childVal
			} else {
				// For other children, evaluate the operator with val as lhs and
				// childVal as rhs.
				switch se.Op {
				case ILLEGAL:
					return 0, fmt.Errorf("encountered illegal op in %d-th subexpression", i)
				case EQ:
					if val == childVal {
						val = 1
					} else {
						val = 0
					}
				case NE:
					if val != childVal {
						val = 1
					} else {
						val = 0
					}
				case GE:
					if val >= childVal {
						val = 1
					} else {
						val = 0
					}
				case LE:
					if val <= childVal {
						val = 1
					} else {
						val = 0
					}
				case GT:
					if val > childVal {
						val = 1
					} else {
						val = 0
					}
				case LT:
					if val < childVal {
						val = 1
					} else {
						val = 0
					}
				case PLUS:
					val = val + childVal
				case MINUS:
					val = val - childVal
				case MUL:
					val = val * childVal
				case DIV:
					if childVal == 0 {
						return 0, fmt.Errorf("division by 0")
					}
					val = val / childVal
				case POW:
					val = int(math.Pow(float64(val), float64(childVal)))
				}
			}
		}
		return val, nil
	case AssignStmt:
		val, err := eval.Eval(nt.Expr)
		if err != nil {
			return 0, err
		}
		eval.symbolTable[nt.Name] = val
		return 0, nil
	case IfStmt:
		condVal, err := eval.Eval(nt.Cond)
		if err != nil {
			return 0, err
		}
		// Lazily evaluate Then or Else based on the result of Cond.
		if condVal == 0 {
			elseVal, err := eval.Eval(nt.Else)
			if err != nil {
				return 0, err
			}
			return elseVal, nil
		} else {
			thenVal, err := eval.Eval(nt.Then)
			if err != nil {
				return 0, err
			}
			return thenVal, nil
		}
	}
	return 0, fmt.Errorf("unmatched node %s", n)
}

func main() {
	srcs := []string{
		`set joe = 2`,
		`if 19 <= 1 + joe ** 3 then 20 else joe * 3`,
	}

	eval := NewEvaluator()
	for i, s := range srcs {
		var p Parser
		n, err := p.Parse(strings.NewReader(s))
		if err != nil {
			log.Printf("Error while parsing '%s': %v", s, err)
		}
		val, err := eval.Eval(n)
		if err != nil {
			log.Printf("Error evaluating '%s': %v", s, err)
		}
		fmt.Printf("%3d: %-50s => %d\n", i, s, val)
	}
}
