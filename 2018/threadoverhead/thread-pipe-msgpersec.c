// Ping ponging messages between two threads on a pipe, measuring number of
// messages sent per second.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

void errExit(const char* s) {
  perror(s);
  exit(EXIT_FAILURE);
}

struct PipeInfo {
  int readfd;
  int writefd;
};

// The child thread spins in a loop reading a 4-byte message from the read pipe
// and echoing it into the write pipe.
void* threadfunc(void* p) {
  struct PipeInfo* pipe_info = (struct PipeInfo*)p;

  char buf[4];
  while (1) {
    int read_rc = read(pipe_info->readfd, buf, 4);
    if (read_rc < 0) {
      errExit("read");
    } else if (read_rc != 4) {
      // If this isn't an error but didn't return 4, assume the pipe is closed
      // and bail out.
      return NULL;
    }
    if (write(pipe_info->writefd, buf, 4) != 4) {
      errExit("write");
    }
  }

  return NULL;
}

int main(int argc, const char** argv) {
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

  int NUM_ITERATIONS = 200000;
  // The message we write into the pipe is "joe" which is 4 bytes long
  // (including the ending \0).
  const char* msg = "joe";
  char buf[4];
  unsigned long long t1 = time_ns();
  for (int i = 0; i < NUM_ITERATIONS; ++i) {
    if (write(main_fds.writefd, msg, 4) != 4) {
      errExit("write");
    }
    if (read(main_fds.readfd, buf, 4) != 4) {
      errExit("read");
    }

    if (strcmp(buf, msg)) {
      printf("ERROR in comparison\n");
      exit(1);
    }
  }
  unsigned long long elapsed = time_ns() - t1;
  printf("%d iterations took %llu ns. %.2lf iters/sec\n", NUM_ITERATIONS,
         elapsed, 1e9 * NUM_ITERATIONS / (double)elapsed);

  // Close parent-side pipe. The child thread will fail at read() at this point
  // and exit, so we can join it.
  close(main_fds.writefd);
  close(main_fds.readfd);

  if (pthread_join(childt, NULL)) {
    errExit("pthread_join");
  }
  return 0;
}
