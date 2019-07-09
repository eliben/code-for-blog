#ifndef CLIBRARY_H
#define CLIBRARY_H

typedef void (*StartCallbackFn)(void* data, int i);
typedef void (*EndCallbackFn)(void* data, int a, int b);

typedef struct {
  StartCallbackFn start;
  EndCallbackFn end;
} Callbacks;

void traverse(void* data, Callbacks cbs);
#endif
