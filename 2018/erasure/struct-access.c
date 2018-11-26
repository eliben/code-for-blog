// First, compile:
//
// $ gcc -c -O3 struct-access.c
//
// Then, disassemble:
//
// $ objdump -d struct-access.o
typedef struct Frob_t {
  int x;
  int y;
  int arr[10];
} Frob;

int extract(Frob* frob) {
  return frob->y * frob->arr[7];
}

int extract_cast(void* p) {
  Frob* frob = p;
  return frob->y * frob->arr[7];
}

int call_extract() {
  Frob ff;
  struct {
    float f, g;
  } fg;
  return extract_cast(&ff) + extract_cast(&fg);
}
