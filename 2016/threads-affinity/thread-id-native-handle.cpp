// Shows how to look at thread IDs and their native handles.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <chrono>
#include <iostream>
#include <mutex>
#include <pthread.h>
#include <thread>
#include <vector>

int main(int argc, const char** argv) {
  std::mutex iomutex;
  std::thread t = std::thread([&iomutex] {
    {
      std::lock_guard<std::mutex> iolock(iomutex);
      std::cout << "Thread: my id = " << std::this_thread::get_id() << "\n"
                << "        my pthread id = " << pthread_self() << "\n";
    }
  });

  {
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << "Launched t: id = " << t.get_id() << "\n"
              << "            native_handle = " << t.native_handle() << "\n";
  }

  t.join();
  return 0;
}
