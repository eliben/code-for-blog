// Measuring thread switching time using a UNIX pipe.
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

static inline long long unsigned time_ns() {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts)) {
    exit(1);
  }
  return ((long long unsigned)ts.tv_sec) * 1000000000LLU +
         (long long unsigned)ts.tv_nsec;
}

void errExit(const char* s) {
  perror(s);
  exit(EXIT_FAILURE);
}

struct PipeInfo {
  int readfd;
  int writefd;
};

// This function accepts a pipe information structure. It then runs a loop for
// num_iterations, where in each iteration it:
//   * Reads a byte from pipeinfo.readfd
//   * Writes a byte to pipeinfo.writefd
//
// Each iteration also incurs two context switches - one to the other thread
// (when read() is called) and one back (when it completes successfully).
void ping_pong(struct PipeInfo pipeinfo, int num_iterations) {
  unsigned id = pthread_self();
  printf("Thread %u ping_pong\n", id);
  printf("  readfd %d; writefd %d\n", pipeinfo.readfd, pipeinfo.writefd);

  char buf[2];
  for (int i = 0; i < num_iterations; ++i) {
    if (read(pipeinfo.readfd, buf, 1) != 1) {
      errExit("read");
    }
    if (write(pipeinfo.writefd, buf, 1) != 1) {
      errExit("write");
    }
  }
}

const int NUM_ITERATIONS = 100000;

void* threadfunc(void* p) {
  struct PipeInfo* pipe_info = (struct PipeInfo*)p;
  ping_pong(*pipe_info, NUM_ITERATIONS);
  return NULL;
}

void measure_self_pipe(int num_iterations) {
  int pfd[2];
  char writebuf[2];
  char readbuf[2];

  if (pipe(pfd) == -1) {
    errExit("pipe");
  }

  // Simple test: write a char to a pipe, test that it arrived as expected.
  writebuf[0] = 'j';
  if (write(pfd[1], writebuf, 1) != 1) {
    errExit("write");
  }
  if (read(pfd[0], readbuf, 1) != 1) {
    errExit("read");
  }
  if (readbuf[0] != 'j') {
    printf("Boo, got %c back from the pipe\n", readbuf[0]);
    exit(1);
  }

  // Now the timing test: in each loop iteration, write a byte into a pipe and
  // then immediately read it.
  const long long unsigned t1 = time_ns();
  for (int i = 0; i < num_iterations; ++i) {
    if (write(pfd[1], writebuf, 1) != 1) {
      errExit("write");
    }
    if (read(pfd[0], readbuf, 1) != 1) {
      errExit("read");
    }
  }
  const long long unsigned elapsed = time_ns() - t1;
  printf("measure_self_pipe: %llu ns for %d iterations (%.2lf ns / iter)\n",
         elapsed, num_iterations, (double)elapsed / num_iterations);
}

int main(int argc, const char** argv) {
  measure_self_pipe(NUM_ITERATIONS);

  // Create two pipes, one for sending data from main to child thread; another
  // for the other direction. Set up the PipeInfo for each side appropriately.
  int main_to_child[2];
  if (pipe(main_to_child) == -1) {
    errExit("pipe");
  }
  int child_to_main[2];
  if (pipe(child_to_main) == -1) {
    errExit("pipe");
  }

  struct PipeInfo main_fds = {.writefd = main_to_child[1],
                              .readfd = child_to_main[0]};
  struct PipeInfo child_fds = {.writefd = child_to_main[1],
                               .readfd = main_to_child[0]};

  pthread_t childt;
  pthread_create(&childt, NULL, threadfunc, (void*)&child_fds);

  // For main, seed the ping-pong by writing a word into the write pipe, since
  // the child will wait for it initially.
  char buf[2] = {'k'};
  if (write(main_fds.writefd, buf, 1) != 1) {
    errExit("write");
  }

  const long long unsigned t1 = time_ns();
  ping_pong(main_fds, NUM_ITERATIONS);
  const long long unsigned elapsed = time_ns() - t1;

  const int nswitches = NUM_ITERATIONS * 2;
  printf("%i context switches in %llu ns (%.1fns / switch)\n", nswitches,
         elapsed, (elapsed / (float)nswitches));

  if (pthread_join(childt, NULL)) {
    errExit("pthread_join");
  }

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
