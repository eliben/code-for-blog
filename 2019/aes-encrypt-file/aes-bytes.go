package main

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"crypto/sha256"
	"fmt"
	"log"
)

func main() {
	// Generate a 256-bit key by hashing an arbitrary password. The key size will
	// affect the strength of the AES cipher used.
	key := sha256.Sum256([]byte("kitty"))

	// Generate a random IV.
	iv := make([]byte, aes.BlockSize)
	if _, err := rand.Read(iv); err != nil {
		log.Fatal(err)
	}

	// Some text to encrypt, making sure its size is a multiple of the AES block
	// size (16).
	text := bytes.Repeat([]byte("i"), 96)

	// Create a new AES block cipher.
	block, err := aes.NewCipher(key[:])
	if err != nil {
		log.Fatal(err)
	}

	// Create a new CBC mode encrypter using our AES block cipher, and use it
	// to encrypt our text.
	ciphertext := make([]byte, len(text))
	enc := cipher.NewCBCEncrypter(block, iv)
	enc.CryptBlocks(ciphertext, text)

	fmt.Println("Ciphertext:", ciphertext)

	newtext := make([]byte, len(ciphertext))
	dec := cipher.NewCBCDecrypter(block, iv)
	dec.CryptBlocks(newtext, ciphertext)

	fmt.Println("Decrypted:", string(newtext))
}
