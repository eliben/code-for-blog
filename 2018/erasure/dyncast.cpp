// $ g++ --rtti --std=c++11 -O3 dyncast.cpp -o dyncast
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <stdio.h>

struct Base {
  virtual void basefunc() {
    printf("basefunc\n");
  }
};

struct Derived : public Base {
  void derivedfunc() {
    printf("derived\n");
  }
};

void call_derived(Base* b) {
  Derived* d = dynamic_cast<Derived*>(b);
  if (d != nullptr) {
    d->derivedfunc();
  } else {
    printf("cast failed\n");
  }
}

int main() {
  Derived d;
  call_derived(&d);

  Base b;
  call_derived(&b);
}
