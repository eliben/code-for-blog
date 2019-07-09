#include <stdio.h>

#include "clibrary.h"

void traverse(void* data, Callbacks cbs) {
  // Simulate some traversal that calls the start callback and then the end
  // callback, if they are defined.
  if (cbs.start != NULL) {
    cbs.start(data, 100);
  }
  if (cbs.end != NULL) {
    cbs.end(data, 2, 3);
  }
}
