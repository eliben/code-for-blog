#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>

// TODO: parse and report vsz here too

void report_memory(const char* prefix) {
  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru)) {
    perror("getrusage");
		exit(1);
  }

	char statusfilename[256];
	snprintf(statusfilename, 256, "/proc/%d/status", getpid());

	FILE* f = fopen(statusfilename, "r");
	if (!f) {
		perror("fopen");
		exit(1);
	}

	char vmsizebuf[256];
	char buf[256];
	while (fgets(buf, 256, f)) {
		if (strstr(buf, "VmSize") == buf) {
			strncpy(vmsizebuf, buf + 7, 255);
			char* pos = strchr(vmsizebuf, '\n');
			if (pos) {
				*pos = '\0';
			}
			break;
		}
	}

	printf("%s: max RSS = %ld; vm size = %s\n", prefix, ru.ru_maxrss, vmsizebuf);
}


int main(int argc, char** argv) {
  printf("PID = %d\n", getpid());
	report_memory("started");

	int N = 100 * 1024 * 1024;
	char* m = malloc(N);
	report_memory("after malloc");

	/*for (int i = 0; i < N; ++i) {*/
		/*m[i] = i;*/
	/*}*/
	/*report_memory("after touch");*/

  printf("press ENTER\n");
  (void)fgetc(stdin);
	return (int)m;
}
