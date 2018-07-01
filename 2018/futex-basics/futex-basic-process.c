#include <linux/futex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>


int futex_safe(int* uaddr, int futex_op, int val,
               const struct timespec* timeout, int* uaddr2, int val3) {
  int rc = syscall(SYS_futex, uaddr, futex_op, val, timeout, uaddr2, val3);
  if (rc < 0) {
    perror("SYS_futex");
    exit(1);
  }
  return rc;
}


int main(int argc, char** argv) {
  int shm_id = shmget(IPC_PRIVATE, 4096, IPC_CREAT | 0666);
  if (shm_id < 0) {
    perror("shmget");
    exit(1);
  }
  int* shared_data = shmat(shm_id, NULL, 0);

  int forkstatus = fork();
  if (forkstatus < 0) {
    perror("fork");
    exit(1);
  }

  if (forkstatus == 0) {
    // Child process

  } else {
    // Parent process.

    // Write 0xA to the shared data.
    *shared_data = 0xA;

    wait(NULL);
    exit(0);
  }


  return 0;
}
