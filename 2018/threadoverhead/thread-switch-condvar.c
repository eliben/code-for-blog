// Measuring thread switching time using a pthread condition variable.
// Two threads signal the same condvar to each other in turns.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static inline unsigned long long time_ns() {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts)) {
    exit(1);
  }
  return ((unsigned long long)ts.tv_sec) * 1000000000LLU +
         (unsigned long long)ts.tv_nsec;
}

// The shared data between parent and child: a condvar with a mutex used to
// protect access to it.
struct SyncInfo {
  pthread_mutex_t mutex;
  pthread_cond_t cv;
};

const int NUM_ITERATIONS = 100000;

void* threadfunc(void* p) {
  struct SyncInfo* si = (struct SyncInfo*)p;

  // The child thread signals first
  pthread_mutex_lock(&si->mutex);
  pthread_cond_signal(&si->cv);
  for (int i = 0; i < NUM_ITERATIONS; ++i) {
    pthread_cond_wait(&si->cv, &si->mutex);
    pthread_cond_signal(&si->cv);
  }
  pthread_mutex_unlock(&si->mutex);
  return NULL;
}

int main(int argc, char** argv) {
  printf("Running for %d iterations\n", NUM_ITERATIONS);

  struct SyncInfo si;
  pthread_mutex_init(&si.mutex, 0);
  pthread_cond_init(&si.cv, 0);

  pthread_mutex_lock(&si.mutex);
  pthread_t childt;
  pthread_create(&childt, NULL, threadfunc, (void*)&si);

  // Each iteration of this loop will switch context from the parent to the
  // child and back - two context switches. The child signals first.
  const long long unsigned start_ns = time_ns();
  for (int i = 0; i < NUM_ITERATIONS; ++i) {
    pthread_cond_wait(&si.cv, &si.mutex);
    pthread_cond_signal(&si.cv);
  }
  pthread_mutex_unlock(&si.mutex);
  const long long unsigned elapsed = time_ns() - start_ns;

  const int nswitches = NUM_ITERATIONS * 2;
  printf("%i context switches in %llu ns (%.1fns / switch)\n", nswitches,
         elapsed, (elapsed / (float)nswitches));

  pthread_join(childt, 0);
  pthread_cond_destroy(&si.cv);
  pthread_mutex_destroy(&si.mutex);

  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru)) {
    perror("getrusage");
  } else {
    printf("From getrusage:\n");
    printf("  voluntary switches = %ld\n", ru.ru_nvcsw);
    printf("  involuntary switches = %ld\n", ru.ru_nivcsw);
  }

  return 0;
}
