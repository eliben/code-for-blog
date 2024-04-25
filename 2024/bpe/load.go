// BPE: loading a BPE vocabulary from a file obtained from OpenAI.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package bpe

import (
	"bufio"
	"encoding/base64"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// LoadTiktokenVocab loads a BPE vocabulary from a publicly-hosted tiktoken
// vocabulary file.
// e.g. https://openaipublic.blob.core.windows.net/encodings/cl100k_base.tiktoken
func LoadTiktokenVocab(r io.Reader) (map[string]int, error) {
	vocab := make(map[string]int)
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			continue
		}

		parts := strings.Split(line, " ")
		if len(parts) != 2 {
			return vocab, fmt.Errorf("want two space-separated parts in line; got %v", line)
		}
		token, err := base64.StdEncoding.DecodeString(parts[0])
		if err != nil {
			return vocab, err
		}
		id, err := strconv.Atoi(parts[1])
		if err != nil {
			return vocab, err
		}
		vocab[string(token)] = id
	}
	if err := scanner.Err(); err != nil {
		return vocab, fmt.Errorf("while scanning input: %v", err)
	}

	return vocab, nil
}
