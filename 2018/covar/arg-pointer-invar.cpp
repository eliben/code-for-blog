// Demonstrates invariance in argument types for derived classes (override).
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>

struct Mammal {
  virtual ~Mammal() = 0;
};

struct Cat : public Mammal {
  virtual ~Cat() {}
};

struct MammalClinic {
  virtual void Accept(Mammal* m);
};

struct CatClinic : public MammalClinic {
  // Note: there should be an 'override' keyword on this method, which would
  // cause a compilation error because this isn't, in fact, an override.
  virtual void Accept(Cat* c);
};

int main(int argc, char** argv) {
  
  return 0;
}
