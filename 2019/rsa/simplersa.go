// Simple implementation of RSA encryption.
// Please use crypto/rsa in real code.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package simplersa

import (
	"bytes"
	"crypto/rand"
	"fmt"
	"math/big"
)

// PublicKey is the public part of an RSA key pair.
type PublicKey struct {
	N *big.Int
	E *big.Int
}

// PrivateKey is the private part of an RSA key pair. According to RFC 2313 we
// could include the prime factors of N and other data here to make decryption
// faster, but N and D are sufficient for decrypting messages.
type PrivateKey struct {
	N *big.Int
	D *big.Int
}

// GenerateKeys generates a public/private key pair for RSA
// encryption/decryption with the given bitlen. See RFC 2313 section 6.
func GenerateKeys(bitlen int) (*PublicKey, *PrivateKey, error) {
	numRetries := 0

	for {
		numRetries++
		if numRetries == 10 {
			panic("retrying too many times, something is wrong")
		}

		// We need a result pq with b bits, so we generate p and q with b/2 bits
		// each. If the top bit of p and q are set, the result will have b bits.
		// Otherwise, we'll retry. rand.Prime should return primes with their top
		// bit set, so in practice there will be no retries.
		p, err := rand.Prime(rand.Reader, bitlen/2)
		if err != nil {
			return nil, nil, err
		}
		q, err := rand.Prime(rand.Reader, bitlen/2)
		if err != nil {
			return nil, nil, err
		}

		// n is pq
		n := new(big.Int).Set(p)
		n.Mul(n, q)

		if n.BitLen() != bitlen {
			continue
		}

		// theta(n) = (p-1)(q-1)
		p.Sub(p, big.NewInt(1))
		q.Sub(q, big.NewInt(1))
		totient := new(big.Int).Set(p)
		totient.Mul(totient, q)

		// e as recommended by PKCS#1 (RFC 2313)
		e := big.NewInt(65537)

		// Calculate the modular multiplicative inverse of e such that:
		//   de = 1 (mod totient)
		// If gcd(e, totient)=1, then e is guaranteed to have a unique inverse, but
		// since p-1 or q-1 could theoretically have e as a factor, this may fail
		// once in a while (likely to be exceedingly rare).
		d := new(big.Int).ModInverse(e, totient)
		if d == nil {
			continue
		}

		pub := &PublicKey{N: n, E: e}
		priv := &PrivateKey{N: n, D: d}
		return pub, priv, nil
	}
}

// encrypt performs encryption of the message m using a public key, and returns
// the encrypted cipher. Encoding the message as a big.Int is the caller's
// responsibility.
func encrypt(pub *PublicKey, m *big.Int) *big.Int {
	c := new(big.Int)
	c.Exp(m, pub.E, pub.N)
	return c
}

// decrypt performs decryption of the cipher c using a private key, and returns
// the decrypted message.
func decrypt(priv *PrivateKey, c *big.Int) *big.Int {
	m := new(big.Int)
	m.Exp(c, priv.D, priv.N)
	return m
}

// EncryptRSA encrypts the message m using public key pub and returns the
// encrypted bytes. The length of m must be <= size_in_bytes(pub.N) - 11,
// otherwise an error is returned. The encryption block format is based on
// PKCS #1 v1.5 (RFC 2313).
func EncryptRSA(pub *PublicKey, m []byte) ([]byte, error) {
	// Compute length of key in bytes, rounding up.
	keyLen := (pub.N.BitLen() + 7) / 8
	if len(m) > keyLen-11 {
		return nil, fmt.Errorf("len(m)=%v, too long", len(m))
	}

	// Following RFC 2313, using block type 02 as recommended for encryption:
	// EB = 00 || 02 || PS || 00 || D
	psLen := keyLen - len(m) - 3
	eb := make([]byte, keyLen)
	eb[0] = 0x00
	eb[1] = 0x02

	// Fill PS with random non-zero bytes.
	for i := 2; i < 2+psLen; {
		_, err := rand.Read(eb[i : i+1])
		if err != nil {
			return nil, err
		}
		if eb[i] != 0x00 {
			i++
		}
	}
	eb[2+psLen] = 0x00

	// Copy the message m into the rest of the encryption block.
	copy(eb[3+psLen:], m)

	// Now the encryption block is complete; we take it as a m-byte big.Int and
	// RSA-encrypt it with the public key.
	mnum := new(big.Int).SetBytes(eb)
	c := encrypt(pub, mnum)

	// The result is a big.Int, which we want to convert to a byte slice of
	// length keyLen. It's highly likely that the size of c in bytes is keyLen,
	// but in rare cases we may need to pad it on the left with zeros (this only
	// happens if the whole MSB of c is zeros, meaning that it's more than 256
	// times smaller than the modulus).
	padLen := keyLen - len(c.Bytes())
	for i := 0; i < padLen; i++ {
		eb[i] = 0x00
	}
	copy(eb[padLen:], c.Bytes())
	return eb, nil
}

// DecryptRSA decrypts the message c using private key priv and returns the
// decrypted bytes, based on block 02 from PKCS #1 v1.5 (RCS 2313).
// It expects the length in bytes of the private key modulo to be len(eb).
// Important: this is a simple implementation not designed to be resilient to
// timing attacks.
func DecryptRSA(priv *PrivateKey, c []byte) ([]byte, error) {
	keyLen := (priv.N.BitLen() + 7) / 8
	if len(c) != keyLen {
		return nil, fmt.Errorf("len(c)=%v, want keyLen=%v", len(c), keyLen)
	}

	// Convert c into a bit.Int and decrypt it using the private key.
	cnum := new(big.Int).SetBytes(c)
	mnum := decrypt(priv, cnum)

	// Write the bytes of mnum into m, left-padding if needed.
	m := make([]byte, keyLen)
	copy(m[keyLen-len(mnum.Bytes()):], mnum.Bytes())

	// Expect proper block 02 beginning.
	if m[0] != 0x00 {
		return nil, fmt.Errorf("m[0]=%v, want 0x00", m[0])
	}
	if m[1] != 0x02 {
		return nil, fmt.Errorf("m[1]=%v, want 0x02", m[1])
	}

	// Skip over random padding until a 0x00 byte is reached. +2 adjusts the index
	// back to the full slice.
	endPad := bytes.IndexByte(m[2:], 0x00) + 2
	if endPad < 2 {
		return nil, fmt.Errorf("end of padding not found")
	}

	return m[endPad+1:], nil
}
