// "go test" doesn't work with 'until' because we haven't fixed the tooling.
package main

import "log"

func testnoinit() {
	i := 4
	count := 0
	until i == 0 {
		i--
		count++
	}

	if count != 4 {
		log.Fatalf("want count 4, got %v", count)
	}
}

func testinit() {
	count := 0
	until i := 3; i == 0 {
		i--
		count++
	}

	if count != 3 {
		log.Fatalf("want count 3, got %v", count)
	}
}

func testnoiters() {
	count := 0
	until i := 3; i == 3 {
		i--
		count++
	}

	if count != 0 {
		log.Fatalf("want count 0, got %v", count)
	}
}

func main() {
	testnoinit()	
	testinit()
	testnoiters()

	log.Println("OK")
}
