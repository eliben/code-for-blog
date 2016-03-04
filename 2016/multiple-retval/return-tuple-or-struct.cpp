// Example of returning tuples or structs.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <string>
#include <tuple>

std::tuple<int, std::string, float> create_a_tuple() {
  return std::make_tuple(20, std::string("baz"), 1.2f);
}

struct RetVal {
  int inumber;
  std::string str;
  float fnumber;
};

RetVal create_a_struct() {
  return {20, std::string("baz"), 1.2f};
}

RetVal create_a_struct_named() {
  return {.inumber = 20, .str = std::string("baz"), .fnumber = 1.2f};
}

struct RetValInitialized {
  int inumber = 17;
  std::string str = "foobar";
  float fnumber = 2.24f;
};

__attribute__((noinline)) RetValInitialized create_an_initialized_struct() {
  return {};
}

struct RetValWithCtor {
  RetValWithCtor(int i)
    : inumber(i), str(i, 'x'), fnumber(i) {}

  int inumber;
  std::string str;
  float fnumber;
};

RetValWithCtor create_a_constructed_struct() {
  return {10};
}

int main(int argc, const char** argv) {
  std::cout << "create_a_tuple\n";
  auto data = create_a_tuple();
  std::cout << "the int: " << std::get<0>(data) << "\n"
            << "the string: " << std::get<1>(data) << "\n"
            << "the float: " << std::get<2>(data) << "\n";

  std::cout << "create_a_tuple with tie\n";
  int i;
  std::string s;
  float f;
  std::tie(i, s, f) = create_a_tuple();
  std::cout << "the int: " << i << "\n"
            << "the string: " << s << "\n"
            << "the float: " << f << "\n";

  std::cout << "create_a_struct\n";
  auto retvaldata = create_a_struct();
  std::cout << "the int: " << retvaldata.inumber << "\n"
            << "the string: " << retvaldata.str << "\n"
            << "the float: " << retvaldata.fnumber << "\n";

  std::cout << "create_an_initialized_struct\n";
  auto rvdi = create_an_initialized_struct();
  std::cout << "the int: " << rvdi.inumber << "\n"
            << "the string: " << rvdi.str << "\n"
            << "the float: " << rvdi.fnumber << "\n";

  std::cout << "create_a_constructed_struct\n";
  auto rvdc = create_a_constructed_struct();
  std::cout << "the int: " << rvdc.inumber << "\n"
            << "the string: " << rvdc.str << "\n"
            << "the float: " << rvdc.fnumber << "\n";

  return 0;
}
