#include <cstdio>

#include "animal.h"


class Sheep : public Animal {
public:
  virtual void say() {
    printf("Sheep says baaaaa\n");
  }

  virtual ~Sheep() {
    printf("Sheep is dead\n");
  }

  void operator delete(void* p) {
    printf("Reclaiming Sheep storage from %p\n", p);
    ::operator delete(p);
  }
};


extern "C" {

Animal* make_a_sheep() {
  return new Sheep;
}

}
