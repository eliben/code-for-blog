// First, compile:
//
// $ gcc -c -O3 struct-access.c
//
// Then, disassemble:
//
// $ objdump -d struct-access.o
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
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
