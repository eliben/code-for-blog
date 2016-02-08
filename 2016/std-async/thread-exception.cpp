// Exceptions in std::threads
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <exception>
#include <functional>
#include <future>
#include <iostream>
#include <numeric>
#include <stdexcept>
#include <thread>
#include <vector>

void accumulate_block_worker(int* data, size_t count, int* result) {
  throw std::runtime_error("something broke");
  *result = std::accumulate(data, data + count, 0);
}

int accumulate_block_worker_ret(int* data, size_t count) {
  throw std::runtime_error("something broke");
  return std::accumulate(data, data + count, 0);
}

void use_worker_in_std_async() {
  std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};
  try {
    std::future<int> fut = std::async(
        std::launch::async, accumulate_block_worker_ret, v.data(), v.size());
    std::cout << "use_worker_in_std_async computed " << fut.get() << "\n";
  } catch (const std::runtime_error& error) {
    std::cout << "caught an error: " << error.what() << "\n";
  }
}

int main(int argc, const char** argv) {
  //std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};
  //int result;
  //try {
    //std::thread worker(accumulate_block_worker,
                       //v.data(), v.size(), &result);
    //worker.join();
    //std::cout << "use_worker_in_std_thread computed " << result << "\n";
  //} catch (const std::runtime_error& error) {
    //std::cout << "caught an error: " << error.what() << "\n";
  //}
  use_worker_in_std_async();

  return 0;
}
