//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <chrono>
#include <iostream>
#include <mutex>
#include <pthread.h>
#include <random>
#include <thread>
#include <vector>

// "Workload function" type. Takes a vector of float data and a reference to
// a single float result.
using WorkloadFunc = std::function<void(const std::vector<float>&, float&)>;

using hires_clock = std::chrono::high_resolution_clock;
using duration_ms = std::chrono::duration<double, std::milli>;

std::mutex iomutex;

void workload_fpchurn(const std::vector<float>& data, float& result) {
  auto t1 = hires_clock::now();
  float rt = 0;
  for (size_t i = 0; i < 10 * 1000 * 1000; ++i) {
    float item = data[i];
    float l = std::log(item);
    if (l > rt) {
      l = std::sin(l);
    } else {
      l = std::cos(l);
    }

    if (l > rt - 2.0) {
      l = std::exp(l);
    } else {
      l = std::exp(l + 1.0);
    }
    rt += l;
  }
  result = rt;

  {
    auto t2 = hires_clock::now();
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << "workload_fpchurn [CPU " << sched_getcpu()
              << "] elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
  }
}

void workload_accum(const std::vector<float>& data, float& result) {
  auto t1 = hires_clock::now();
  float rt = 0;
  for (size_t count = 0; count < 5; ++count) {
    for (size_t i = 0; i < data.size(); ++i) {
      rt += data[i];
    }
  }
  result = rt;

  {
    auto t2 = hires_clock::now();
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << "workload_accum [CPU " << sched_getcpu()
              << "] elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
  }
}

std::vector<float> make_input_array(int N) {
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<float> dis(-1.0f, 1.0f);

  std::vector<float> vf(N);
  for (size_t i = 0; i < vf.size(); ++i) {
    vf[i] = dis(gen);
  }

  return vf;
}

// This function can be used to mark memory that shouldn't be optimized away,
// without actually generating any code.
void do_not_optimize(void* p) {
  asm volatile("" : : "g"(p) : "memory");
}

void pin_thread_to_cpu(std::thread& t, int cpu_num) {
  cpu_set_t cpuset;
  CPU_ZERO(&cpuset);
  CPU_SET(cpu_num, &cpuset);
  int rc =
      pthread_setaffinity_np(t.native_handle(), sizeof(cpu_set_t), &cpuset);
  if (rc != 0) {
    std::cerr << "Error calling pthread_setaffinity_np: " << rc << "\n";
  }
}

int main(int argc, const char** argv) {
  auto t1 = hires_clock::now();
  std::vector<float> vf = make_input_array(100 * 1000 * 1000);
  std::cout << "Created input array of size " << vf.size()
            << "; elapsed: " << duration_ms(hires_clock::now() - t1).count()
            << " ms\n";
  
  // argv[0] is the program name
  // argv[1] is workload name, with cpu number at argv[2]
  // argv[3] is workload name, with cpu number at argv[4]
  // ... etc.

  int num_workloads = argc / 2;
  std::vector<float> results(num_workloads);
  do_not_optimize(results.data());
  std::vector<std::thread> threads(num_workloads);

  for (int i = 1; i < argc; i += 2) {
    WorkloadFunc func;
    std::string workload_name = argv[i];
    if (workload_name == "fpchurn") {
      func = workload_fpchurn;
    } else if (workload_name == "accum") {
      func = workload_accum;
    }

    int cpu_num;
    if (i + 1 >= argc) {
      cpu_num = 0;
    } else {
      cpu_num = std::atoi(argv[i + 1]);
    }

    {
      std::lock_guard<std::mutex> iolock(iomutex);
      std::cout << "Spawning workload '" << workload_name << "' on CPU "
                << cpu_num << "\n";
    }

    int nworkload = i / 2;
    threads[nworkload] =
        std::thread(func, std::cref(vf), std::ref(results[nworkload]));
    pin_thread_to_cpu(threads[nworkload], cpu_num);
  }

  std::for_each(threads.begin(), threads.end(),
                std::mem_fn(&std::thread::join));
  return 0;
}
