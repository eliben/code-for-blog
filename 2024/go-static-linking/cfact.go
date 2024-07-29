package main

// static inline int fact(int arg) {
//   int n = 1;
//   for (int i = 1; i <= arg; ++i) {
//     n *= i;
//   }
//   return n;
// }
import "C"
import "fmt"

func main() {
	fmt.Println("Calculating factorial in C")
	fmt.Println(C.fact(8))
}
