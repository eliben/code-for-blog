// Covariance and contravariance in std::function.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
#include <functional>

struct Vertebrate {};
struct Mammal : public Vertebrate {};
struct Cat : public Mammal {};

Cat* f1(Vertebrate* v) {
  return nullptr;
}

Vertebrate* f2(Vertebrate* v) {
  return nullptr;
}

Cat* f3(Cat* v) {
  return nullptr;
}

void User(std::function<Mammal*(Mammal*)> f) {
  // do stuff with 'f'
}

int main() {
  User(f1);       // works
  //User(f2);       // error: Vertebrate cannot be converted to Mammal
  //User(f3):       // error: Mammal cannot be converted to Cat

  return 0;
}
