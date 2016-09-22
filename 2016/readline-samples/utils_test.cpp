// Common utils for readline samples - tests.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include "utils.h"

#include <cassert>
#include <iostream>

void test_longest_common_prefix() {
  assert(longest_common_prefix("foo", {"foob", "foobar"}) == "foob");
  assert(longest_common_prefix("foo", {"foob"}) == "foob");
  assert(longest_common_prefix("foo", {"foo", "foobar"}) == "foo");
  assert(longest_common_prefix("k", {"kbc1", "kbc2", "kbc2"}) == "kbc");
  assert(longest_common_prefix("k", {"kbc1"}) == "kbc1");
  assert(longest_common_prefix("k", {"kbc1", "kbb", "kbc2"}) == "kb");
  assert(longest_common_prefix("", {"kbc1", "kbb", "kbc2"}) == "kb");
  assert(longest_common_prefix("k", {"kba1", "kba2", "kba3"}) == "kba");
}

int main(int argc, const char** argv) {
#ifdef NDEBUG
  std::cout << "NDEBUG defined: assertions disabled\n";
#endif

  test_longest_common_prefix();
  std::cout << "OK!\n";
  return 0;
}
