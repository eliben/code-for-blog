// Example of using the hwloc portable API to query the system's topology and
// to pin the calling thread to a given logical CPU.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
//
#include <chrono>
#include <cstdlib>
#include <iostream>
#include <sched.h>
#include <thread>

#include <hwloc.h>


void report_thread_cpu() {
  std::this_thread::sleep_for(std::chrono::milliseconds(50));
  std::cout << "Thread id " << std::this_thread::get_id() << ", running on CPU "
            << sched_getcpu() << "\n";
}

int main() {
  report_thread_cpu();

  // Initialize hwloc.
  hwloc_topology_t topology;
  if (hwloc_topology_init(&topology) < 0) {
    std::cerr << "error calling hwloc_topology_init\n";
    std::exit(1);
  }

  if (hwloc_topology_load(topology) < 0) {
    std::cerr << "error calling hwloc_topology_load\n";
    std::exit(1);
  }

  // Describe the machine's HW topology, on each depth level.
  int topodepth = hwloc_topology_get_depth(topology);
  std::cout << "Depth of topology = " << topodepth << "\n";

  for (int depth = 0; depth < topodepth; depth++) {
    int num_objects = hwloc_get_nbobjs_by_depth(topology, depth);
    std::cout << "Level " << depth << ": " << num_objects << " objects\n";
    for (int i = 0; i < num_objects; i++) {
      hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);
      char s[512];
      hwloc_obj_type_snprintf(s, sizeof(s), obj, 1);
      std::cout << " " << i << ": " << s << "\n";
    }
  }

  // Now pin the calling thread to the last PU (logical CPU) on the system.
  int num_objects_last_depth =
      hwloc_get_nbobjs_by_depth(topology, topodepth - 1);
  hwloc_obj_t pu = hwloc_get_obj_by_depth(topology, topodepth - 1,
                                          num_objects_last_depth - 1);

  std::cout << "Pinning thread to last PU; OS index = " << pu->os_index << "\n";
  if (hwloc_set_cpubind(topology, pu->cpuset, HWLOC_CPUBIND_THREAD) < 0) {
    std::cerr << "Error calling hwloc_set_cpubind\n";
    std::exit(1);
  }

  report_thread_cpu();
  return 0;
}
