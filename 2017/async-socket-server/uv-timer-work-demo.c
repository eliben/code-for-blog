// Demo of dispatching sleeping/blocking in a callback to a work queue.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <uv.h>

void on_after_work(uv_work_t* req, int status) {
  free(req);
}

void on_work(uv_work_t* req) {
  // "Work"
  if (random() % 5 == 0) {
    printf("Sleeping...\n");
    sleep(3);
  }
}

void on_timer(uv_timer_t* timer) {
  uint64_t timestamp = uv_hrtime();
  printf("on_timer [%" PRIu64 " ms]\n", (timestamp / 1000000) % 100000);

  uv_work_t* work_req = (uv_work_t*)malloc(sizeof(*work_req));
  uv_queue_work(uv_default_loop(), work_req, on_work, on_after_work);
}

int main(int argc, const char** argv) {
  uv_timer_t timer;
  uv_timer_init(uv_default_loop(), &timer);
  uv_timer_start(&timer, on_timer, 0, 1000);
  return uv_run(uv_default_loop(), UV_RUN_DEFAULT);
}
