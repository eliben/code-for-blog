#include <iostream>

class BinaryPlus;
class Constant;

class ExprVisitor {
public:
  virtual void VisitConstant(const Constant& c) const = 0;
  virtual void VisitBinaryPlus(const BinaryPlus& c) const = 0;
};

class Expr {
public:
  virtual void Visit(ExprVisitor* visitor) = 0;
};

class Constant : public Expr {
public:
  Constant(double value) : value_(value) {}

  void Visit(ExprVisitor* visitor) const {
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

  void Visit(ExprVisitor* visitor) const {
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

int main(int argc, const char** argv) {

  return 0;
}
