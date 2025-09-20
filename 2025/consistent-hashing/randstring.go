package main

import "math/rand/v2"

const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

func generateRandomString(rnd *rand.Rand, length int) string {
	b := make([]byte, length)
	for i := range b {
		b[i] = charset[rnd.IntN(len(charset))]
	}
	return string(b)
}
