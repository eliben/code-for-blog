// This package has some type errors, so the tooling should report them somehow
// when the package is loaded.
package main

func util(x int) {
}

func main() {
	util(5 6)

	var s string = 5.5
}
