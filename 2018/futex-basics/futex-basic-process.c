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

int main(int argc, char** argv) {
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
    // Wait until 0xA is written to the shared data, taking spurious wake-ups
    // into account.
    int blocked = 1;
    while (blocked) {
      int futex_rc = futex(shared_data, FUTEX_WAIT, 0xA, NULL, NULL, 0);
      if (futex_rc == -1) {
        if (errno != EAGAIN) {
          perror("futex");
          exit(1);
        }
      } else if (futex_rc == 0) {
        if (*shared_data == 0xA) {
          blocked = 0;
        }
      } else {
        abort();
      }
    }

    printf("child writing B\n");
    // Write 0xB to the shared data and send a wakeup call.
    *shared_data = 0xB;
    while (futex(shared_data, FUTEX_WAKE, 1, NULL, NULL, 0) == 0) {
      sched_yield();
    }
  } else {
    // Parent process.

    printf("parent writing A\n");
    // Write 0xA to the shared data and send a wakeup call (until someone is
    // actually woken up).
    *shared_data = 0xA;
    while (futex(shared_data, FUTEX_WAKE, 1, NULL, NULL, 0) == 0) {
      sched_yield();
    }

    printf("parent waiting for B\n");
    // Wait until 0xB is written to the shared data, taking spurious wake-ups
    // into account.
    int blocked = 1;
    while (blocked) {
      int futex_rc = futex(shared_data, FUTEX_WAIT, 0xB, NULL, NULL, 0);
      if (futex_rc == -1) {
        if (errno != EAGAIN) {
          perror("futex");
          exit(1);
        }
      } else if (futex_rc == 0) {
        if (*shared_data == 0xB) {
          blocked = 0;
        }
      } else {
        abort();
      }
    }

    // Wait for the child to terminate.
    wait(NULL);
  }

  return 0;
}
