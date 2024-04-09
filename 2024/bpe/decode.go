package bpe

type Decoder struct {
	token2str map[int]string
}

func NewDecoder(vocab map[string]int) *Decoder {
	token2str := make(map[int]string)
	for k, v := range vocab {
		token2str[v] = k
	}
	return &Decoder{
		token2str: token2str,
	}
}

func (d *Decoder) decode(tokens []int) []string {
	var s []string
	for _, t := range tokens {
		s = append(s, d.token2str[t])
	}
	return s
}
