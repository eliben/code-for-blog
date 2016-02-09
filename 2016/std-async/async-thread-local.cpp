// Mixing thread_local with std::async launches.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <chrono>
#include <functional>
#include <future>
#include <iostream>
#include <numeric>
#include <thread>
#include <vector>

thread_local int tls_var;

template <typename F, typename... Ts>
inline auto reallyAsync(F&& f, Ts&&... params) {
  return std::async(std::launch::async, std::forward<F>(f),
                    std::forward<Ts>(params)...);
}

int read_tls_var() {
  return tls_var;
}

int main(int argc, const char** argv) {
  tls_var = 50;

  std::future<int> fut = reallyAsync(read_tls_var);
  std::cout << "got from read_tls_var: " << fut.get() << "\n";
  return 0;
}
