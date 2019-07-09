#include <stdio.h>

#include "clibrary.h"

void traverse(char* filename, Callbacks cbs, void* user_data) {
  // Simulate some traversal that calls the start callback and then the end
  // callback, if they are defined.
  if (cbs.start != NULL) {
    cbs.start(user_data, 100);
  }
  if (cbs.end != NULL) {
    cbs.end(user_data, 2, 3);
  }
}
