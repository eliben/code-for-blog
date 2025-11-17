__attribute__((noinline))
int add_three(int x, int y, int z) {
  return x + y + z;
}

int call_add_three(int t) {
  int x = t * 2;
  int y = t - 112;
  int z = t + 393;
  return add_three(x, y, z);
}

__attribute__((noinline))
char add_three_chars(char x, char y, char z) {
  return x + y + z;
}

char call_add_three_chars(char t) {
  char x = t + 2;
  char y = t - 12;
  char z = t + 3;
  return add_three_chars(x, y, z);
}

__int128 add128(__int128 a, __int128 b) {
  return a + b;
}

int add_indirect(int x, int y, int* sum) {
    *sum = x + y;
    return *sum;
}
