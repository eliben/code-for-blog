package main

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/binary"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

func encryptFile(key []byte, filename string, outFilename string) (string, error) {
	if len(outFilename) == 0 {
		outFilename = filename + ".enc"
	}

	plaintext, err := ioutil.ReadFile(filename)
	if err != nil {
		return "", err
	}

	of, err := os.Create(outFilename)
	if err != nil {
		return "", err
	}
	defer of.Close()

	// Write the original plaintext size into the output file first, encoded in
	// a 8-byte integer.
	origSize := uint64(len(plaintext))
	if err = binary.Write(of, binary.LittleEndian, origSize); err != nil {
		return "", err
	}

	// Pad plaintext to a multiple of BlockSize with space characters.
	if len(plaintext)%aes.BlockSize != 0 {
		bytesToPad := aes.BlockSize - (len(plaintext) % aes.BlockSize)
		plaintext = append(plaintext, bytes.Repeat([]byte(" "), bytesToPad)...)
	}

	// Generate random IV and write it to the output file.
	iv := make([]byte, aes.BlockSize)
	if _, err := rand.Reader.Read(iv); err != nil {
		return "", err
	}
	if _, err = of.Write(iv); err != nil {
		return "", err
	}

	ciphertext := make([]byte, len(plaintext))

	block, err := aes.NewCipher(key)
	if err != nil {
		return "", err
	}

	mode := cipher.NewCBCEncrypter(block, iv)
	mode.CryptBlocks(ciphertext, plaintext)

	if _, err = of.Write(ciphertext); err != nil {
		return "", err
	}
	return outFilename, nil
}

func main() {
	encFlag := flag.Bool("e", false, "encrypt")
	decFlag := flag.Bool("d", false, "decrypt")

	flag.Parse()
	filename := flag.Arg(0)

	key := bytes.Repeat([]byte("1"), 32)
	if *encFlag {
		outFilename, err := encryptFile(key, filename, "")
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println("Encrypted output file:", outFilename)
	} else if *decFlag {

	}
}
