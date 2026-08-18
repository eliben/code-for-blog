package server

import "time"

// IsPrime returns true if n is prime, false otherwise. If delay is true, it
// will sleep for n milliseconds before calculating.
func IsPrime(n int64, delay bool) bool {
	if delay {
		time.Sleep(time.Duration(n) * time.Millisecond)
	}

	if n < 2 {
		return false
	}
	if n%2 == 0 {
		return n == 2
	}
	for i := int64(3); i*i <= n; i += 2 {
		if n%i == 0 {
			return false
		}
	}
	return true
}
