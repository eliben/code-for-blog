// Extended visitor pattern approach.
//
// Attempt to implement the extended visitor approach to add a data type to a
// typical visitor-based solution; based on the Krishnamurthi paper.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <cassert>
#include <cstdlib>
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

class FunctionCall;

// To be able to visit FunctionCall expressions, extend the ExprVisitor
// interface.
class ExprVisitorWithFunctionCall : virtual public ExprVisitor {
public:
  virtual void VisitFunctionCall(const FunctionCall& fc) = 0;
};

// This is the new ("extended") expression we're adding.
class FunctionCall : public Expr {
public:
  FunctionCall(const std::string& name, const Expr& argument)
      : name_(name), argument_(argument) {}

  void Accept(ExprVisitor* visitor) const {
    ExprVisitorWithFunctionCall* v =
        dynamic_cast<ExprVisitorWithFunctionCall*>(visitor);
    if (v == nullptr) {
      std::cerr << "Fatal: visitor is not ExprVisitorWithFunctionCall\n";
      exit(1);
    }
    v->VisitFunctionCall(*this);
  }

private:
  std::string name_;
  const Expr& argument_;
};

class Evaluator : virtual public ExprVisitor {
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

// This class implements the ExprVisitorWithFunctionCall interface, and takes
// most of its functionality from the existing Evaluator class. Therefore, we're
// using multiple inheritance. This is also why the base classes have to inherit
// virtually from ExprVisitor; otherwise, we'd end up with two different
// ExprVisitor instances at the base of this class and the code wouldn't compile
// as the compiler can't figure out that this class in fact implements the
// ExprVisitor interface.
class EvaluatorWithFunctionCall : public ExprVisitorWithFunctionCall,
                                  public Evaluator {
public:
  void VisitFunctionCall(const FunctionCall& fc) {
    std::cout << "Visiting FunctionCall!!\n";
  }
};

int main(int argc, const char** argv) {
  std::unique_ptr<Expr> c1(new Constant(1.1));
  std::unique_ptr<Expr> c2(new Constant(2.2));

  std::unique_ptr<Expr> fc(new FunctionCall("foo", *c1));
  std::unique_ptr<Expr> p2(new BinaryPlus(*fc, *c2));

  EvaluatorWithFunctionCall e;
  p2->Accept(&e);
  std::cout << e.GetValueForExpr(*p2) << "\n";

  return 0;
}
