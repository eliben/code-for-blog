#include <cstdio>
#include <dlfcn.h>

#include "animal.h"


int main(int argc, const char *argv[]) {
  void *libhandle = dlopen(argv[1], RTLD_LAZY);
  if (!libhandle) {
    fprintf(stderr, "dlopen error: %s\n", dlerror());
    return 1;
  }

  printf("dlopen success: handle %p\n", libhandle);

  animal_factory factory =
      reinterpret_cast<animal_factory>(dlsym(libhandle, "make_a_sheep"));
  const char *err = dlerror();
  if (err) {
    fprintf(stderr, "dlsym failed: %s\n", err);
    return 1;
  }

  Animal* animal = factory();
  animal->say();
  delete animal;

  return 0;
}
