// Demo of sleeping/blocking in a callback, and its effect on the event loop.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <uv.h>

void on_timer(uv_timer_t* timer) {
  uint64_t timestamp = uv_hrtime();
  printf("on_timer [%" PRIu64 " ms]\n", (timestamp / 1000000) % 100000);

  // "Work"
  if (random() % 5 == 0) {
    printf("Sleeping...\n");
    sleep(3);
  }
}

int main(int argc, const char** argv) {
  uv_timer_t timer;
  uv_timer_init(uv_default_loop(), &timer);
  uv_timer_start(&timer, on_timer, 0, 1000);
  return uv_run(uv_default_loop(), UV_RUN_DEFAULT);
}
