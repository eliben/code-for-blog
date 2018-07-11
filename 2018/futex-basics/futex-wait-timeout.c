// Futex sample that uses the timeout parameter.
//
// Basic userspace handshake using futexes, for two processes.
// Try with timeout > 500 ms to see timeouts in the child.
// Then try with timeout < 500 ms to see wait returning without a timeout.
//
// Eli Bendersky [http://eli.thegreenplace.net]
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

// A blocking wrapper for waking a futex. Only returns when a waiter has been
// woken up.
void wake_futex_blocking(int* futex_addr) {
  while (1) {
    int futex_rc = futex(futex_addr, FUTEX_WAKE, 1, NULL, NULL, 0);
    if (futex_rc == -1) {
      perror("futex wake");
      exit(1);
    } else if (futex_rc > 0) {
      return;
    }
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
      int futex_rc = futex(shared_data, FUTEX_WAIT, 0xA, &timeout, NULL, 0);
      printf("child woken up rc=%d errno=%s, elapsed=%llu\n", futex_rc,
             futex_rc ? strerror(errno) : "", time_ns() - t1);
      if (futex_rc == 0 && *shared_data == 0xA) {
        break;
      }
    }
  } else {
    // Parent process.

    printf("parent writin A\n");
    *shared_data = 0xA;
    usleep(atoi(argv[1]) * 1000);

    printf("parent waking child\n");
    // Wake up child.
    wake_futex_blocking(shared_data);

    // Wait for the child to terminate.
    wait(NULL);
  }

  return 0;
}
