#define _GNU_SOURCE
#include <sched.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int child_func(void* arg) {
  return 0;
}

int main(int argc, char** argv) {
  // Stack for child.
  const int STACK_SIZE = 65536;

  char* stack = malloc(STACK_SIZE);
  if (!stack) {
    perror("malloc");
    exit(1);
  }

  unsigned long flags = 0;
  if (clone(child_func, stack + STACK_SIZE, flags | SIGCHLD, NULL) == -1) {
    perror("clone");
    exit(1);
  }

  int status;
  pid_t pid = waitpid(-1, &status, 0);
  if (pid == -1) {
    perror("waitpid");
    exit(1);
  }

  printf("    Child PID=%ld\n", (long)pid);

  return 0;
}
