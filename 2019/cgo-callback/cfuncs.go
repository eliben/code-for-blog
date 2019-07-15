// Wrappers for Go callback functions to be passed into C.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

/*
extern void goStart(void*, int);
extern void goEnd(void*, int, int);

void startCgo(void* user_data, int i) {
  goStart(user_data, i);
}

void endCgo(void* user_data, int a, int b) {
  goEnd(user_data, a, b);
}
*/
import "C"
