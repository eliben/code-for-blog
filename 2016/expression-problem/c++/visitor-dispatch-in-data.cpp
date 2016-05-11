// Visitor pattern approach.
//
// Here we do generic dispatch in the data classes (like the visits in
// BinaryPlus). A more flexible way is to leave it to the visitor (like it's
// done in the Krishnamurthi paper). See visitor-dispatch-in-visitor.cpp for
// that.
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
    lhs_.Accept(visitor);
    rhs_.Accept(visitor);
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
    // Assuming the lhs and rhs of bp were already evaluated; otherwise 0 is
    // assumed.
    value_map_[&bp] = value_map_[&(bp.GetLhs())] + value_map_[&(bp.GetRhs())];
  }

private:
  std::map<const Expr*, double> value_map_;
};

class Stringifier : public ExprVisitor {
public:
  std::string GetStringForExpr(const Expr& e) {
    return value_map_[&e];
  }

  void VisitConstant(const Constant& c) {
    std::ostringstream ss;
    ss << c.GetValue();
    value_map_[&c] = ss.str();
  }

  void VisitBinaryPlus(const BinaryPlus& bp) {
    // Assuming the lhs and rhs of bp were already evaluated; otherwise an empty
    // string is assumed.
    value_map_[&bp] =
        value_map_[&(bp.GetLhs())] + " + " + value_map_[&(bp.GetRhs())];
  }

private:
  std::map<const Expr*, std::string> value_map_;
};

int main(int argc, const char** argv) {
  std::unique_ptr<Expr> c1(new Constant(1.1));
  std::unique_ptr<Expr> c2(new Constant(2.2));

  std::unique_ptr<Expr> p1(new BinaryPlus(*c1, *c2));
  std::unique_ptr<Expr> p2(new BinaryPlus(*p1, *c2));

  Stringifier s;
  p2->Accept(&s);
  std::cout << s.GetStringForExpr(*p2) << "\n";

  Evaluator e;
  p2->Accept(&e);
  std::cout << e.GetValueForExpr(*p2) << "\n";

  return 0;
}
