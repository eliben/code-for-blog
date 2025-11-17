unsigned add_three(unsigned x, unsigned y, unsigned z) {
  return x + y + z;
}

unsigned add_indirect(unsigned x, unsigned y, unsigned* sum) {
    *sum = x + y;
    return *sum;
}
