package main

// #include <stdio.h>
// void helloworld() {
//   printf("hello, world from C\n");
// }
import "C"

func main() {
	C.helloworld()
}
