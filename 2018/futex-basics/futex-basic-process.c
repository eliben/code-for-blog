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

// Waits for the futex at futex_addr to have the value val, ignoring spurious
// wakeups. This function only returns when the condition is fulfilled; the only
// other way out is aborting with an error.
void wait_on_futex_value(int* futex_addr, int val) {
  while (1) {
    int futex_rc = futex(futex_addr, FUTEX_WAIT, val, NULL, NULL, 0);
    if (futex_rc == -1) {
      if (errno != EAGAIN) {
        perror("futex");
        exit(1);
      }
      sched_yield();
    } else if (futex_rc == 0) {
      if (*futex_addr == val) {
        return;
      }
    } else {
      abort();
    }
  }
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
    wait_on_futex_value(shared_data, 0xA);

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
    wait_on_futex_value(shared_data, 0xB);

    // Wait for the child to terminate.
    wait(NULL);
  }

  return 0;
}
