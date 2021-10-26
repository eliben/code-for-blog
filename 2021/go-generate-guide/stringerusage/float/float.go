// Demonstrates an enum used as input for the `stringer` generator.
//
// This code was taken from the Go distribution (math.big package); it is
// subject to Go's license.

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
