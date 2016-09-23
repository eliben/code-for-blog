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

void test_tokenize_line_buffer() {
  std::vector<Token> t1 = tokenize_line_buffer("one \ttwo three");
  assert(t1.size() == 3);
  assert(t1[0].text == "one");
  assert(t1[0].buf_index == 0);
  assert(t1[1].text == "two");
  assert(t1[1].buf_index == 5);
  assert(t1[2].text == "three");
  assert(t1[2].buf_index == 9);

  std::vector<Token> t2 = tokenize_line_buffer("  ");
  assert(t2.size() == 0);

  std::vector<Token> t3 = tokenize_line_buffer(" \t\tfoo  ");
  assert(t3.size() == 1);
  assert(t3[0].text == "foo");
  assert(t3[0].buf_index == 3);
}

int main(int argc, const char** argv) {
#ifdef NDEBUG
  std::cout << "NDEBUG defined: assertions disabled\n";
#endif

  test_longest_common_prefix();
  test_tokenize_line_buffer();

  std::cout << "OK!\n";
  return 0;
}
