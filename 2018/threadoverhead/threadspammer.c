// Simple tool for measuring resource usage of a large number of idle threads
// running simultaneously.
//
// Run it with number of threads:
//
// $ ./threadspammer 10000
//
// Will launch 10000 threads then wait for pressing <ENTER>.
//
// And watch the process's resource usage via top or other tools.
//
// Experiments: observe virtual memory vs. resident memory with different thread
// number settings and stack size settings.
// Set USE_MEMORY env var (to any value) to make the thread function actually
// use the memory.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>

// This function is used to prevent optimizations by the compiler for p, while
// compiling to no instructions. It lets the compiler believe that p may be used
// for updating all memory
void escape(void* p) {
  asm volatile("" : : "g"(p) : "memory");
}

void* threadfunc(void* p) {
	int m[100 * 1024];
	pthread_t tid = pthread_self();
  escape(m);
	if (getenv("USE_MEMORY") != 0) {
		for (int i = 0; i < 100 * 1024; ++i) {
			m[i] = tid % 1000000 + i;
		}
	}
  // Sleep for 10 seconds total.
  for (int i = 0; i < 10 * 20; ++i) {
    usleep(50 * 1000);
  }
  return NULL;
}

int main(int argc, const char** argv) {
  if (argc != 2) {
    printf("Usage: %s <numthreads>\n", argv[0]);
    exit(1);
  }
  int nthreads = atoi(argv[1]);

  printf("PID = %d\n", getpid());
  printf("Running with nthreads = %d\n", nthreads);

  pthread_attr_t attr;
  /*size_t stacksize = 100 * 1024;*/
  pthread_attr_init(&attr);
  /*pthread_attr_setstacksize(&attr, stacksize);*/

  for (long i = 0; i < nthreads; ++i) {
    pthread_t t;

    pthread_create(&t, &attr, threadfunc, (void*)i);
    usleep(50);
  }

  printf("press ENTER\n");
  (void)fgetc(stdin);

  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru)) {
    perror("getrusage");
  } else {
    printf("From getrusage:\n");
    printf("  max RSS = %ld kB\n", ru.ru_maxrss);
  }

  return 0;
}
