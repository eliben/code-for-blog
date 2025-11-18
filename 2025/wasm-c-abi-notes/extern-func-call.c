__attribute__((import_module("env"), import_name("get_stuff")))
int get_stuff();

int do_work(int x) {
  int stuffs[64];
  for (int i = 0; i < 64; i++) {
    stuffs[i] = get_stuff();
  }

  int index = get_stuff();
  return stuffs[index];
}
