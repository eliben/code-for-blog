#include <memory>
#include <iostream>
#include <sstream>

// Expr abstract inferface.
class Expr {
public:
  virtual std::string ToString() const = 0;
  virtual double Eval() const = 0;
};

class Constant : public Expr {
public:
  Constant(double value) : value_(value) {}

  std::string ToString() const {
    std::ostringstream ss;
    ss << value_;
    return ss.str();
  }

  double Eval() const {
    return value_;
  }

private:
  double value_;
};

class BinaryPlus : public Expr {
public:
  BinaryPlus(const Expr* lhs, const Expr* rhs) : lhs_(lhs), rhs_(rhs) {}

  std::string ToString() const {
    return lhs_->ToString() + " + " + rhs_->ToString();
  }

  double Eval() const {
    return lhs_->Eval() + rhs_->Eval();
  }

private:
  const Expr* lhs_;
  const Expr* rhs_;
};

int main(int argc, const char** argv) {
  std::unique_ptr<Expr> c1(new Constant(1.1));
  std::unique_ptr<Expr> c2(new Constant(2.2));

  std::unique_ptr<Expr> p(new BinaryPlus(c1.get(), c2.get()));

  std::cout << p->ToString() << "\n";
  std::cout << p->Eval() << "\n";

  return 0;
}
