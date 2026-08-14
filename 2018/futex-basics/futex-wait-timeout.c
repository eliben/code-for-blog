// Futex sample that uses the timeout parameter.
//
// Basic userspace handshake using futexes, for two processes.
// Try with parent delay > 500 ms to see timeouts in the child.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
#include <errno.h>
#include <linux/futex.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/shm.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

int futex(int* uaddr, int futex_op, int val, const struct timespec* timeout,
          int* uaddr2, int val3) {
  return syscall(SYS_futex, uaddr, futex_op, val, timeout, uaddr2, val3);
}

static inline unsigned long long time_ns() {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts)) {
    exit(1);
  }
  return ((long long unsigned)ts.tv_sec) * 1000000000LLU +
         (long long unsigned)ts.tv_nsec;
}

// Attempt to wake a futex; this doesn't guarantee anything was woken - it's
// possible that nobody is waiting on the futex yet.
void wake_futex(int* futex_addr) {
  int futex_rc = futex(futex_addr, FUTEX_WAKE, 1, NULL, NULL, 0);
  if (futex_rc == -1) {
    perror("futex wake");
    exit(1);
  }
}

int main(int argc, char** argv) {
  if (argc < 2) {
    printf("Usage: %s <parent wait in ms before waking child>\n", argv[0]);
    exit(1);
  }

  int shm_id = shmget(IPC_PRIVATE, 4096, IPC_CREAT | 0666);
  if (shm_id < 0) {
    perror("shmget");
    exit(1);
  }
  int* shared_data = shmat(shm_id, NULL, 0);
  if (shared_data == (void*)-1) {
    perror("shmat");
    exit(1);
  }
  *shared_data = 0;

  int forkstatus = fork();
  if (forkstatus < 0) {
    perror("fork");
    exit(1);
  }

  if (forkstatus == 0) {
    // Child process

    printf("child waiting for A\n");
    struct timespec timeout = {.tv_sec = 0, .tv_nsec = 500000000};
    while (1) {
      unsigned long long t1 = time_ns();
      int observed_val = *shared_data;
      if (observed_val == 0xA) {
        break;
      }
      int futex_rc =
          futex(shared_data, FUTEX_WAIT, observed_val, &timeout, NULL, 0);
      printf("child wait returned rc=%d errno=%s, elapsed=%llu\n", futex_rc,
             futex_rc ? strerror(errno) : "", time_ns() - t1);
      if (futex_rc == -1 && errno != EAGAIN && errno != EINTR &&
          errno != ETIMEDOUT) {
        perror("FUTEX_WAIT");
        exit(1);
      }
    }
  } else {
    // Parent process.

    printf("parent writing A\n");
    usleep(atoi(argv[1]) * 1000);
    *shared_data = 0xA;

    printf("parent waking child\n");
    // Wake up child.
    wake_futex(shared_data);

    // Wait for the child to terminate.
    wait(NULL);
  }

  return 0;
}
