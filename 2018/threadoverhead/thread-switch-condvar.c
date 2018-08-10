#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

const int NUM_ITERS = 100000;

static inline unsigned long long time_ns() {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts)) {
    exit(1);
  }
  return ((unsigned long long)ts.tv_sec) * 1000000000LLU +
         (unsigned long long)ts.tv_nsec;
}

struct sync_info {
  pthread_mutex_t mutex;
  pthread_cond_t cv;
};

void* threadfunc(void* p) {
  struct sync_info* si = (struct sync_info*)p;

  // The child thread signals first
  pthread_mutex_lock(&si->mutex);
  pthread_cond_signal(&si->cv);
  for (int i = 0; i < NUM_ITERS; ++i) {
    pthread_cond_wait(&si->cv, &si->mutex);
    pthread_cond_signal(&si->cv);
  }
  pthread_mutex_unlock(&si->mutex);
  return NULL;
}

int main(int argc, char** argv) {
  printf("Running for %d iterations\n", NUM_ITERS);

  struct sync_info si;
  pthread_mutex_init(&si.mutex, 0);
  pthread_cond_init(&si.cv, 0);

  pthread_mutex_lock(&si.mutex);
  pthread_t childt;
  pthread_create(&childt, NULL, threadfunc, (void*)&si);

  const long long unsigned start_ns = time_ns();
  for (int i = 0; i < NUM_ITERS; ++i) {
    pthread_cond_wait(&si.cv, &si.mutex);
    pthread_cond_signal(&si.cv);
  }
  pthread_mutex_unlock(&si.mutex);
  const long long unsigned delta = time_ns() - start_ns;

  pthread_join(childt, 0);
  pthread_cond_destroy(&si.cv);
  pthread_mutex_destroy(&si.mutex);

  const int nswitches = NUM_ITERS * 2;
  printf("%i process context switches in %12lluns (%.1fns/ctxsw)\n",
         nswitches, delta, (delta / (float)nswitches));

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
