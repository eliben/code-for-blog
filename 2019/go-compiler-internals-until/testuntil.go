// Some tests for 'until.
// "go test" doesn't work with 'until' because we haven't fixed the tooling.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
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

func testbreak() {
	count := 0
	until i := 100; i == 0 {
		if i == 40 {
			break
		}
		count++
		i--
	}

	if count != 60 {
		log.Fatalf("want count 60, got %v", count)
	}
}

func testcontinue() {
	count := 0
	until i := 100; i == 0 {
		i--
		count++
		if i > 25 {
			continue
		}
		break
	}
	if count != 75 {
		log.Fatalf("want count 75, got %v", count)
	}
}

func testemptycond() {
	i := 100
	count := 0
	until {
		i--
		count++
		if i == 45 {
			break
		}
	}
	if count != 55 {
		log.Fatalf("want count 55, got %v", count)
	}
}

func main() {
	testnoinit()
	testinit()
	testnoiters()
	testbreak()
	testcontinue()
	testemptycond()

	log.Println("OK")
}
