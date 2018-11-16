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
