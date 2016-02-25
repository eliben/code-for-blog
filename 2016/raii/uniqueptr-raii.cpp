// unique_ptr RAII example
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <memory>

struct BigData {
  void do_stuff() {}
  bool do_other_stuff(int) {return true;}
};

// For compilers that don't have C++14 / make_unique yet
namespace std {
template <typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}
}

void using_big_data() {
  std::unique_ptr<BigData> data(new BigData);

  data->do_stuff();

  if (data->do_other_stuff(42)) {
    return;
  }

  data->do_stuff();
}

void using_big_data2() {
  auto data = std::make_unique<BigData>();

  data->do_stuff();

  if (data->do_other_stuff(42)) {
    return;
  }

  data->do_stuff();
}

int main(int argc, const char** argv) {
  using_big_data();
  using_big_data2();

  return 0;
}
