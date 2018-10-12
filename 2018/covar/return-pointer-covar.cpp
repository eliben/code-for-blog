// Demonstrates covariance in pointer return types for derived classes.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>

struct Mammal {
  virtual ~Mammal() = 0;
  virtual Mammal* Clone() = 0;
};

struct Cat : public Mammal {
  virtual ~Cat() {}

  Cat* Clone() override {
    return new Cat(*this);
  }
};

struct Dog : public Mammal {
  virtual ~Dog() {}

  Dog* Clone() override {
    return new Dog(*this);
  }
};

Mammal* DoSomething(Mammal* m) {
  Mammal* cloned = m->Clone();
  // Do something with cloned
  return cloned;
}

int main(int argc, const char** argv) {
  return 0;
}
