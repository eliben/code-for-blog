package float

type RoundingMode byte

const (
	ToNearestEven RoundingMode = iota
	ToNearestAway
	ToZero
	AwayFromZero
	ToNegativeInf
	ToPositiveInf
)

//go:generate stringer -type=RoundingMode
