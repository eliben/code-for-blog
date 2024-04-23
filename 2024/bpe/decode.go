package bpe

// Decoder decodes a list of token IDs into a list of text elements that can
// be concatenated together to produce the original text.
type Decoder struct {
	token2str map[int]string
}

// NewDecoder creates a new Decoder from the given vocabulary.
func NewDecoder(vocab map[string]int) *Decoder {
	token2str := make(map[int]string)
	for k, v := range vocab {
		token2str[v] = k
	}
	return &Decoder{
		token2str: token2str,
	}
}

// Decode a list of tokens into a list of strings these tokens represent.
func (d *Decoder) Decode(tokens []int) []string {
	var s []string
	for _, t := range tokens {
		s = append(s, d.token2str[t])
	}
	return s
}
