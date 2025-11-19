struct Pair {
    unsigned x;
    unsigned y;
};

__attribute__((noinline))
struct Pair make_pair(unsigned x, unsigned y) {
  struct Pair pp = {.x = x, .y = y};
  return pp;
}

unsigned do_work(unsigned x, unsigned y) {
  struct Pair pp = make_pair(x, y);
  return 7*pp.x + 3*pp.y;
}
