// Comprehensive benchmark demonstrating sharing of cores for different
// workloads.
//
// When built, accepts an arbitrary number of argument pairs: (workload, cpu)
// and launches the given workloads in separate threads on the given CPUs.
//
// For example:
//
//   ./thread-workload-benchmark accum 0 sin 1
//
// Launches the "accum" workload in on (logical) CPU 0, and the "sin" workload
// in parallel on CPU 1. Each workload is given its own (separately allocated)
// vector of 100 million floats.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <algorithm>
#include <chrono>
#include <cmath>
#include <iostream>
#include <mutex>
#include <pthread.h>
#include <random>
#include <thread>
#include <vector>

// "Workload function" type. Takes a vector of float data and a reference to
// a single float result.
using WorkloadFunc = std::function<void(const std::vector<float>&, float&)>;

// Some aliases to make std::chrono calls shorter.
using hires_clock = std::chrono::high_resolution_clock;
using duration_ms = std::chrono::duration<double, std::milli>;

std::mutex iomutex;

void workload_fpchurn(const std::vector<float>& data, float& result) {
  constexpr size_t NUM_ITERS = 10 * 1000 * 1000;
  auto t1 = hires_clock::now();
  float rt = 0;
  for (size_t i = 0; i < NUM_ITERS; ++i) {
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
    std::cout << __func__ << " [CPU " << sched_getcpu() << "]:\n";
    std::cout << "  elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
  }
}

void workload_sin(const std::vector<float>& data, float& result) {
  auto t1 = hires_clock::now();
  float rt = 0;
  for (size_t i = 0; i < data.size(); ++i) {
    rt += std::sin(data[i]);
  }
  result = rt;

  {
    auto t2 = hires_clock::now();
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << __func__ << " [cpu " << sched_getcpu() << "]:\n";
    std::cout << "  processed items: " << data.size() << "\n";
    std::cout << "  elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
    std::cout << "  result: " << result << "\n";
  }
}

void workload_accum(const std::vector<float>& data, float& result) {
  auto t1 = hires_clock::now();
  float rt = 0;
  for (size_t i = 0; i < data.size(); ++i) {
    // On x86-64, this should generate a loop of data.size ADDSS instructions,
    // all adding up into the same xmm register. If built with -Ofast
    // (-ffast-math), the compiler will be willing to perform unsafe FP
    // optimizations and will vectorize the loop into data.size/4 ADDPS
    // instructions. Note that this changes the order in which the floats are
    // added, which is unsafe because FP addition is not associative.
    rt += data[i];
  }
  result = rt;

  {
    auto t2 = hires_clock::now();
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << __func__ << " [cpu " << sched_getcpu() << "]:\n";
    std::cout << "  processed items: " << data.size() << "\n";
    std::cout << "  elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
    std::cout << "  result: " << result << "\n";
  }
}

void workload_unrollaccum4(const std::vector<float>& data, float& result) {
  auto t1 = hires_clock::now();
  if (data.size() % 4 != 0) {
    std::cerr
        << "ERROR in " << __func__ << ": data.size " << data.size() << "\n";
  }
  float rt0 = 0;
  float rt1 = 0;
  float rt2 = 0;
  float rt3 = 0;
  for (size_t i = 0; i < data.size(); i += 4) {
    // This unrolling performs manual break-down of dependencies on a single xmm
    // register that happens in workload_accum. It should be faster because
    // distinct ADDSS instructions will accumulate into separate registers. It
    // is also unsafe for the same reasons as described above.
    rt0 += data[i];
    rt1 += data[i + 1];
    rt2 += data[i + 2];
    rt3 += data[i + 3];
  }
  result = rt0 + rt1 + rt2 + rt3;

  {
    auto t2 = hires_clock::now();
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << __func__ << " [cpu " << sched_getcpu() << "]:\n";
    std::cout << "  processed items: " << data.size() << "\n";
    std::cout << "  elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
    std::cout << "  result: " << result << "\n";
  }
}

void workload_unrollaccum8(const std::vector<float>& data, float& result) {
  auto t1 = hires_clock::now();
  if (data.size() % 8 != 0) {
    std::cerr
        << "ERROR in " << __func__ << ": data.size " << data.size() << "\n";
  }
  float rt0 = 0;
  float rt1 = 0;
  float rt2 = 0;
  float rt3 = 0;
  float rt4 = 0;
  float rt5 = 0;
  float rt6 = 0;
  float rt7 = 0;
  for (size_t i = 0; i < data.size(); i += 8) {
    rt0 += data[i];
    rt1 += data[i + 1];
    rt2 += data[i + 2];
    rt3 += data[i + 3];
    rt4 += data[i + 4];
    rt5 += data[i + 5];
    rt6 += data[i + 6];
    rt7 += data[i + 7];
  }
  result = rt0 + rt1 + rt2 + rt3 + rt4 + rt5 + rt6 + rt7;

  {
    auto t2 = hires_clock::now();
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << __func__ << " [cpu " << sched_getcpu() << "]:\n";
    std::cout << "  processed items: " << data.size() << "\n";
    std::cout << "  elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
    std::cout << "  result: " << result << "\n";
  }
}

void workload_stdaccum(const std::vector<float>& data, float& result) {
  auto t1 = hires_clock::now();
  result = std::accumulate(data.begin(), data.end(), 0.0f);

  {
    auto t2 = hires_clock::now();
    std::lock_guard<std::mutex> iolock(iomutex);
    std::cout << __func__ << " [cpu " << sched_getcpu() << "]:\n";
    std::cout << "  processed items: " << data.size() << "\n";
    std::cout << "  elapsed: " << duration_ms(t2 - t1).count() << " ms\n";
    std::cout << "  result: " << result << "\n";
  }
}

// Create a vector filled with N floats uniformly distributed in the range
// (-1.0,1.0)
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

// Set the given thread's affinity to be exclusively on the given logical CPU
// number.
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
  // Set locale for comma-separation in large numbers.
  std::cout.imbue(std::locale(""));
  
  // Command-line invocation:
  //
  //  argv[0] is the program name
  //  argv[1] is workload name, with cpu number at argv[2]
  //  argv[3] is workload name, with cpu number at argv[4]
  //  ... etc.

  int num_workloads = argc / 2;
  std::vector<float> results(num_workloads);
  do_not_optimize(results.data());
  std::vector<std::thread> threads(num_workloads);

  constexpr size_t INPUT_SIZE = 100 * 1000 * 1000;
  auto t1 = hires_clock::now();

  // Allocate and initialize one extra input array - not used by any workload.
  // This makes sure none of the actual working inputs stays in L3 cache (which
  // is fairly sizable), giving one of the workloads an unfair advantage. The
  // lower cache layers are so small compared to the input size that their
  // pre-seeding advantage is negligible.
  std::vector<std::vector<float>> inputs(num_workloads + 1);
  for (int i = 0; i < num_workloads; ++i) {
    inputs[i] = make_input_array(INPUT_SIZE);
  }
  std::cout << "Created " << num_workloads + 1 << " input arrays"
            << "; elapsed: " << duration_ms(hires_clock::now() - t1).count()
            << " ms\n";

  for (int i = 1; i < argc; i += 2) {
    WorkloadFunc func;
    std::string workload_name = argv[i];
    if (workload_name == "fpchurn") {
      func = workload_fpchurn;
    } else if (workload_name == "sin") {
      func = workload_sin;
    } else if (workload_name == "accum") {
      func = workload_accum;
    } else if (workload_name == "unrollaccum4") {
      func = workload_unrollaccum4;
    } else if (workload_name == "unrollaccum8") {
      func = workload_unrollaccum8;
    } else if (workload_name == "stdaccum") {
      func = workload_stdaccum;
    } else {
      std::cerr << "unknown workload: " << argv[i] << "\n";
      return 1;
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
    threads[nworkload] = std::thread(func, std::cref(inputs[nworkload]),
                                     std::ref(results[nworkload]));
    pin_thread_to_cpu(threads[nworkload], cpu_num);
  }

  // All the threads were launched in parallel in the loop above. Now wait for
  // all of them to finish.
  for (auto& t : threads) {
    t.join();
  }
  return 0;
}
