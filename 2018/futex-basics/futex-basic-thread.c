// Same as futex-basic-process.c, but using threads.
//
// Main differences:
// 1. No need for shared memory calls (shmget, etc.), since threads share the
//    address space we can use a simple pointer.
// 2. Use the _PRIVATE versions of system calls since these can be more
//    efficient within a single process.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <errno.h>
#include <linux/futex.h>
#include <pthread.h>
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

// The C runtime doesn't provide a wrapper for the futex(2) syscall, so we roll
// our own.
int futex(int* uaddr, int futex_op, int val, const struct timespec* timeout,
          int* uaddr2, int val3) {
  return syscall(SYS_futex, uaddr, futex_op, val, timeout, uaddr2, val3);
}

// Waits for the futex at futex_addr to have the value val, ignoring spurious
// wakeups. This function only returns when the condition is fulfilled; the only
// other way out is aborting with an error.
void wait_on_futex_value(int* futex_addr, int val) {
  while (1) {
    int futex_rc = futex(futex_addr, FUTEX_WAIT_PRIVATE, val, NULL, NULL, 0);
    if (futex_rc == -1) {
      if (errno != EAGAIN) {
        perror("futex");
        exit(1);
      }
    } else if (futex_rc == 0) {
      if (*futex_addr == val) {
        // This is a real wakeup.
        return;
      }
    } else {
      abort();
    }
  }
}

// A blocking wrapper for waking a futex. Only returns when a waiter has been
// woken up.
void wake_futex_blocking(int* futex_addr) {
  while (1) {
    int futex_rc = futex(futex_addr, FUTEX_WAKE_PRIVATE, 1, NULL, NULL, 0);
    if (futex_rc == -1) {
      perror("futex wake");
      exit(1);
    } else if (futex_rc > 0) {
      return;
    }
  }
}

void* threadfunc(void* p) {
  int* shared_data = (int*)p;
  printf("child waiting for A\n");
  wait_on_futex_value(shared_data, 0xA);

  printf("child writing B\n");
  // Write 0xB to the shared data and wake up parent.
  *shared_data = 0xB;
  wake_futex_blocking(shared_data);
  return NULL;
}

int main(int argc, char** argv) {
  int futex_var = 0;
  int* shared_data = &futex_var;

  pthread_t childt;
  pthread_create(&childt, NULL, threadfunc, (void*)shared_data);

  // Parent thread.

  printf("parent writing A\n");
  // Write 0xA to the shared data and wake up child.
  *shared_data = 0xA;
  wake_futex_blocking(shared_data);

  printf("parent waiting for B\n");
  wait_on_futex_value(shared_data, 0xB);

  pthread_join(childt, NULL);
}
