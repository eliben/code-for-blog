#include <functional>
#include <map>
#include <memory>
#include <iostream>
#include <sstream>
#include <string>

typedef std::map<std::string, double> SymbolTable;

class Expr {
public:
  virtual double Eval(SymbolTable* st = nullptr) const = 0;
};

class Constant : public Expr {
public:
  Constant(double value) : value_(value) {}

  double Eval(SymbolTable* st = nullptr) const {
    return value_;
  }

private:
  double value_;
};

class VarRef : public Expr {
public:
  VarRef(const char* varname) : varname_(varname) {}

  double Eval(SymbolTable* st = nullptr) const {
    return (*st)[varname_];
  }

private:
  std::string varname_;
};

typedef std::function<double(double, double)> BinaryFunction;

class BinaryOp : public Expr {
public:
  BinaryOp(BinaryFunction func, const Expr& lhs, const Expr& rhs)
      : func_(func), lhs_(lhs), rhs_(rhs) {}

  double Eval(SymbolTable* st = nullptr) const {
    return func_(lhs_.Eval(st), rhs_.Eval(st));
  }

private:
  BinaryFunction func_;
  const Expr& lhs_;
  const Expr& rhs_;
};

int main(int argc, const char** argv) {
  std::unique_ptr<Expr> c1(new Constant(2.0));
  std::unique_ptr<Expr> c2(new Constant(3.3));
  std::unique_ptr<Expr> v(new VarRef("x"));

  SymbolTable st{{"x", 1.1}};

  std::unique_ptr<Expr> e1(new BinaryOp(std::multiplies<double>(), *c1, *c2));
  std::unique_ptr<Expr> e2(new BinaryOp(std::plus<double>(), *e1, *v));

  std::cout << e2->Eval(&st) << "\n";

  return 0;
}
