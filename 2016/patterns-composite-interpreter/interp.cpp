// Sample interpreter for showing the Composite and Interpreter design patterns.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <functional>
#include <map>
#include <memory>
#include <iostream>
#include <sstream>
#include <string>

// Maps symbol names to their values. An expression is evaluated in the context
// of a symbol table, in order to assign concrete values to variables referenced
// within it.
typedef std::map<std::string, double> SymbolTable;

class Expr {
public:
  virtual double Eval(SymbolTable* st) const = 0;
};

class Constant : public Expr {
public:
  Constant(double value) : value_(value) {}

  double Eval(SymbolTable* st) const {
    return value_;
  }

private:
  double value_;
};

class VarRef : public Expr {
public:
  VarRef(const char* varname) : varname_(varname) {}

  double Eval(SymbolTable* st) const {
    return (*st)[varname_];
  }

private:
  std::string varname_;
};

// A function type for computing the result of a binary operation.
typedef std::function<double(double, double)> BinaryFunction;

class BinaryOp : public Expr {
public:
  BinaryOp(BinaryFunction func, const Expr& lhs, const Expr& rhs)
      : func_(func), lhs_(lhs), rhs_(rhs) {}

  double Eval(SymbolTable* st) const {
    return func_(lhs_.Eval(st), rhs_.Eval(st));
  }

private:
  BinaryFunction func_;
  const Expr& lhs_;
  const Expr& rhs_;
};

int main(int argc, const char** argv) {
  // Define a couple of constants and a reference to the variable 'A'.
  std::unique_ptr<Expr> c1(new Constant(2.0));
  std::unique_ptr<Expr> c2(new Constant(3.3));
  std::unique_ptr<Expr> v(new VarRef("A"));

  // Define a binary expression representing "2.0 * 3.3 + A"
  std::unique_ptr<Expr> e1(new BinaryOp(std::multiplies<double>(), *c1, *c2));
  std::unique_ptr<Expr> e2(new BinaryOp(std::plus<double>(), *e1, *v));

  // Evaluate in the context of a symbol table where A has the value 1.1
  SymbolTable st{{"A", 1.1}};
  std::cout << e2->Eval(&st) << "\n";

  return 0;
}
