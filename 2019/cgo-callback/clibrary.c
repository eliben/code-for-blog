#include <stdio.h>

#include "clibrary.h"

void traverse(void* data, Callbacks cbs) {
  if (cbs.start) {
    printf("calling start\n");
    cbs.start(data, 100);
  }
  if (cbs.end) {
    printf("calling end\n");
    cbs.end(data, 2, 3);
  }
  printf("returning\n");
}
