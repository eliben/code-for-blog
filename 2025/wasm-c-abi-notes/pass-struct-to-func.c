struct Pair {
    unsigned x;
    unsigned y;
};

__attribute__((noinline))
unsigned pair_add(struct Pair pair) {
    return pair.x + pair.y;
}

unsigned do_work(unsigned x, unsigned y){
    struct Pair pp = {.x = x, .y = y};
    return pair_add(pp);
}
