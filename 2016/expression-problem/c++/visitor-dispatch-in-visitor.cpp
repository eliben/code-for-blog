// Visitor pattern approach.
//
// Here each data class just calls the appropriate Visit* method on the visitor,
// without worrying about visiting its constituent expressions. That is left to
// the actual visitors.
//
// Pros: more flexible - the visitors know what constituents they want to visit,
// in what order and how to combine the results.
// Cons: code duplication - if there are many visitors, they all replicate the
// same visiting logic most of the time.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>

class BinaryPlus;
class Constant;

class ExprVisitor {
public:
  virtual void VisitConstant(const Constant& c) = 0;
  virtual void VisitBinaryPlus(const BinaryPlus& bp) = 0;
};

class Expr {
public:
  virtual void Accept(ExprVisitor* visitor) const = 0;
};

class Constant : public Expr {
public:
  Constant(double value) : value_(value) {}

  void Accept(ExprVisitor* visitor) const {
    visitor->VisitConstant(*this);
  }

  double GetValue() const {
    return value_;
  }

private:
  double value_;
};

class BinaryPlus : public Expr {
public:
  BinaryPlus(const Expr& lhs, const Expr& rhs) : lhs_(lhs), rhs_(rhs) {}

  void Accept(ExprVisitor* visitor) const {
    visitor->VisitBinaryPlus(*this);
  }

  const Expr& GetLhs() const {
    return lhs_;
  }

  const Expr& GetRhs() const {
    return rhs_;
  }

private:
  const Expr& lhs_;
  const Expr& rhs_;
};

class Evaluator : public ExprVisitor {
public:
  double GetValueForExpr(const Expr& e) {
    return value_map_[&e];
  }

  void VisitConstant(const Constant& c) {
    value_map_[&c] = c.GetValue();
  }

  void VisitBinaryPlus(const BinaryPlus& bp) {
    bp.GetLhs().Accept(this);
    bp.GetRhs().Accept(this);
    value_map_[&bp] = value_map_[&(bp.GetLhs())] + value_map_[&(bp.GetRhs())];
  }

private:
  std::map<const Expr*, double> value_map_;
};

int main(int argc, const char** argv) {
  std::unique_ptr<Expr> c1(new Constant(1.1));
  std::unique_ptr<Expr> c2(new Constant(2.2));

  std::unique_ptr<Expr> p1(new BinaryPlus(*c1, *c2));
  std::unique_ptr<Expr> p2(new BinaryPlus(*p1, *c2));

  Evaluator e;
  p2->Accept(&e);
  std::cout << e.GetValueForExpr(*p2) << "\n";

  return 0;
}
