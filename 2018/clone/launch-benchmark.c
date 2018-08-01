// Measure launch overhead / join time for processes (fork) or threads
// (pthread_create), with optional malloc-ing before the launch to assess
// effects on launch time.
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

// Memory we're going to optionally allocating with malloc prioir to launching a
// task. Marked volatile to thwart compiler optimizations.
volatile char* v;

#define min(a, b)                                                              \
  ({                                                                           \
    __typeof__(a) _a = (a);                                                    \
    __typeof__(b) _b = (b);                                                    \
    _a < _b ? _a : _b;                                                         \
  })

void* threadfunc(void* p) {
  (void)p;
  return 0;
}

void die_with_usage(char** argv) {
  printf("Usage: %s <thread|fork> <mem size in MB>\n", argv[0]);
  exit(0);
}

int main(int argc, char** argv) {
  printf("pid = %d\n", getpid());
  if (argc < 3) {
    die_with_usage(argv);
  }

  // If use_thread = 0, measuring pthread launch performance; otherwise
  // measuring fork performance.
  int use_thread = 0;
  if (!strcmp(argv[1], "thread")) {
    use_thread = 1;
  } else if (!strcmp(argv[1], "fork")) {
    use_thread = 0;
  } else {
    die_with_usage(argv);
  }

  int MEMSIZEMB = atoi(argv[2]);
  if (MEMSIZEMB > 0) {
    // Allocate and touch memory to make sure it's paged in.
    int MEMSIZE = MEMSIZEMB * 1e6;
    v = malloc(MEMSIZE);
    if (!v) {
      perror("malloc");
      exit(1);
    }
    for (int i = 0; i < MEMSIZE; ++i) {
      v[i] = i;
    }
  }

  int N = 10000;
  unsigned long long minlaunch = 999999;
  unsigned long long totallaunch = 0;
  unsigned long long minjoin = 999999;
  unsigned long long totaljoin = 0;

  for (int i = 0; i < N; ++i) {
    pthread_t tid;

    unsigned long long t1 = time_ns();
    if (use_thread) {
      if (pthread_create(&tid, NULL, threadfunc, NULL)) {
        perror("pthread_create");
        exit(1);
      }
    } else {
      if (fork() == 0) {
        // Child. Just exit.
        exit(0);
      }
      // Parent. Fall through to measurement.
    }
    // Parent process, or thread.
    unsigned long long elapsed = time_ns() - t1;
    minlaunch = min(minlaunch, elapsed);
    totallaunch += elapsed;

    // Let some time pass to not count the actual child running time in
    // elapsed for the teardown.
    usleep(500);

    t1 = time_ns();
    if (use_thread) {
      if (pthread_join(tid, NULL)) {
        perror("pthread_join");
        exit(1);
      }
    } else {
      if (wait(NULL) == -1) {
        perror("wait");
        exit(1);
      }
    }
    elapsed = time_ns() - t1;
    minjoin = min(minjoin, elapsed);
    totaljoin += elapsed;
  }

  printf("After %d iterations:\n  minlaunch = %llu\n  minjoin = %llu\n", N,
         minlaunch, minjoin);
  printf("Average:\n  launch = %.2lf\n  join = %.2lf\n",
         (double)totallaunch / N, (double)totaljoin / N);

  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru)) {
    perror("getrusage");
  } else {
    printf("From getrusage:\n");
    printf("  max rss (KiB): %ld\n", ru.ru_maxrss);
  }

  printf("Press <enter> to exit.\n");
  getchar();
  return 0;
}
