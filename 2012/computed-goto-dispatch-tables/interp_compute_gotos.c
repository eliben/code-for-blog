// Simple interpreter loop with a switch vs. computed goto.
//
// Requires a file named "zz.bin" to be present in the working directory,
// for benchmarking. See the accompanying fill_file.py script for generating
// such a file.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define OP_HALT 0x0
#define OP_INC 0x1
#define OP_DEC 0x2
#define OP_MUL2 0x3
#define OP_DIV2 0x4
#define OP_ADD7 0x5
#define OP_NEG 0x6

int interp_switch(unsigned char* code, int initval) {
  int pc = 0;
  int val = initval;

  while (1) {
    switch (code[pc++]) {
    case OP_HALT:
      return val;
    case OP_INC:
      val++;
      break;
    case OP_DEC:
      val--;
      break;
    case OP_MUL2:
      val *= 2;
      break;
    case OP_DIV2:
      val /= 2;
      break;
    case OP_ADD7:
      val += 7;
      break;
    case OP_NEG:
      val = -val;
      break;
    default:
      return val;
    }
  }
}

int interp_cgoto(unsigned char* code, int initval) {
  /* The indices of labels in the dispatch_table are the relevant opcodes
   */
  static void* dispatch_table[] = {&&do_halt, &&do_inc,  &&do_dec, &&do_mul2,
                                   &&do_div2, &&do_add7, &&do_neg};
#define DISPATCH() goto* dispatch_table[code[pc++]]

  int pc = 0;
  int val = initval;

  DISPATCH();
do_halt:
  return val;
do_inc:
  val++;
  DISPATCH();
do_dec:
  val--;
  DISPATCH();
do_mul2:
  val *= 2;
  DISPATCH();
do_div2:
  val /= 2;
  DISPATCH();
do_add7:
  val += 7;
  DISPATCH();
do_neg:
  val = -val;
  DISPATCH();
}

/* Expecting at least a 20 MiB file */
#define DSIZE 20000000
unsigned char* prepare_data() {
  unsigned char* data = malloc(DSIZE);
  FILE* f = fopen("zz.bin", "rb");
  if (!f)
    return 0;
  if (fread(data, 1, DSIZE, f) != DSIZE)
    return 0;
  data[DSIZE - 1] = '\x00';
  return data;
}

int main(int argc, const char* argv[]) {
  // Small test
  unsigned char* tc = (unsigned char*)"\x01\x01\x03\x06\x02\x02\x04\x05\x00";
  int result = interp_switch(tc, 1);
  printf("Result = %d\n", result);
  result = interp_cgoto(tc, 1);
  printf("Goto result = %d\n", result);

  {
    clock_t starttime, endtime;
    unsigned char* data = prepare_data();

    starttime = clock();
    result = interp_switch(data, 1);
    endtime = clock();
    printf("interp = %d [elapsed: %lf]\n", result,
           ((double)endtime - starttime) / CLOCKS_PER_SEC);

    starttime = clock();
    result = interp_cgoto(data, 1);
    endtime = clock();
    printf("cgoto = %d [elapsed: %lf]\n", result,
           ((double)endtime - starttime) / CLOCKS_PER_SEC);
  }

  return 0;
}
