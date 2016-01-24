// Sample of launching C++11 threads and checking how many CPUs the host system
// has.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <chrono>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>

int main(int argc, const char** argv) {
  unsigned num_cpus = std::thread::hardware_concurrency();
  std::cout << "Launching " << num_cpus << " threads\n";

  // A mutex ensures orderly access to std::cout from multiple threads.
  std::mutex iomutex;
  std::vector<std::thread> threads(num_cpus);
  for (unsigned i = 0; i < num_cpus; ++i) {
    threads[i] = std::thread([&iomutex, i] {
      {
        // Use a lexical scope and lock_guard to safely lock the mutex only for
        // the duration of std::cout usage.
        std::lock_guard<std::mutex> iolock(iomutex);
        std::cout << "Thread #" << i << " is running\n";
      }

      // Simulate important work done by the tread by sleeping for a bit...
      std::this_thread::sleep_for(std::chrono::milliseconds(200));

    });
  }

  for (auto& t : threads) {
    t.join();
  }
  return 0;
}
