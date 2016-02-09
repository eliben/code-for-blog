// Mutex locking/unlocking with std::async launches.
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

int task(std::recursive_mutex& m) {
  m.lock();
  return 42;
}

int main(int argc, const char** argv) {
  std::recursive_mutex m;
  m.lock();

  std::future<int> fut = std::async(std::launch::deferred, task, std::ref(m));
  std::cout << "got from task: " << fut.get() << "\n";
  return 0;
}
