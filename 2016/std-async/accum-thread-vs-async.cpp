// Sample of launching C++11 threads with std::thread vs. std::async.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <functional>
#include <future>
#include <iostream>
#include <numeric>
#include <thread>
#include <vector>

void accumulate_block_worker(int* data, size_t count, int* result) {
  *result = std::accumulate(data, data + count, 0);
}

void use_worker_in_std_thread() {
  std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};
  int result;
  std::thread worker(accumulate_block_worker,
                     v.data(), v.size(), &result);
  worker.join();
  std::cout << "use_worker_in_std_thread computed " << result << "\n";
}

// Demonstrates how to launch two threads and return two results to the caller
// that will have to wait on those threads.
std::vector<std::thread>
launch_split_workers_with_std_thread(std::vector<int>& v,
                                     std::vector<int>* results) {
  std::vector<std::thread> threads;
  threads.emplace_back(accumulate_block_worker, v.data(), v.size() / 2,
                       &((*results)[0]));
  threads.emplace_back(accumulate_block_worker, v.data() + v.size() / 2,
                       v.size() / 2, &((*results)[1]));
  return threads;
}

int accumulate_block_worker_ret(int* data, size_t count) {
  return std::accumulate(data, data + count, 0);
}

void use_worker_in_std_async() {
  std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};
  std::future<int> fut = std::async(
      std::launch::async, accumulate_block_worker_ret, v.data(), v.size());
  std::cout << "use_worker_in_std_async computed " << fut.get() << "\n";
}

using int_futures = std::vector<std::future<int>>;

// Demonstrates how to launch two computation threads with std::async.
int_futures launch_split_workers_with_std_async(std::vector<int>& v) {
  int_futures futures;
  futures.push_back(std::async(std::launch::async, accumulate_block_worker_ret,
                               v.data(), v.size() / 2));
  futures.push_back(std::async(std::launch::async, accumulate_block_worker_ret,
                               v.data() + v.size() / 2, v.size() / 2));
  return futures;
}

int main(int argc, const char** argv) {
  use_worker_in_std_thread();
  use_worker_in_std_async();

  std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};
  std::vector<int> results(2, 0);
  std::vector<std::thread> threads =
      launch_split_workers_with_std_thread(v, &results);
  for (auto& t : threads) {
    t.join();
  }
  std::cout << "results from launch_split_workers_with_std_thread: "
            << results[0] << " and " << results[1] << "\n";

  int_futures futures = launch_split_workers_with_std_async(v);
  std::cout << "results from launch_split_workers_with_std_async: "
            << futures[0].get() << " and " << futures[1].get() << "\n";

  return 0;
}
