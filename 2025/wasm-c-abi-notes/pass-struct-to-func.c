struct Pair {
    unsigned x;
    unsigned y;
};

__attribute__((noinline))
unsigned pair_calculate(struct Pair pair) {
    return 7*pair.x + 3*pair.y;
}

__attribute__((noinline))
unsigned do_work(unsigned x, unsigned y){
    struct Pair pp = {.x = x, .y = y};
    return pair_calculate(pp);
}


__attribute__((import_module("env"), import_name("get_num")))
unsigned get_num();

unsigned toplevel() {
  struct Pair pairs[32];
  struct Pair selected_pair;
  pairs[get_num()].x = get_num();
  pairs[get_num()].y = get_num();
  selected_pair = pairs[get_num()];

  return do_work(selected_pair.x, selected_pair.y);
}
