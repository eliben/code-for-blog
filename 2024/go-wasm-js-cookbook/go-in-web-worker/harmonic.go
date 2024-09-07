//go:build js && wasm

package main

import (
	"math/big"
	"syscall/js"
	"time"
)

func main() {
	js.Global().Set("calcHarmonic", jsCalcHarmonic)

	select {}
}

// calcHarmonic calculates the harmonic series for approximately the given
// number of seconds and returns the accumulated result in a string.
func calcHarmonic(nsecs float64) string {
	d := time.Duration(nsecs * float64(time.Second))
	start := time.Now()
	r1 := big.NewRat(1, 1)
	for i := 2; ; i++ {
		addend := big.NewRat(1, int64(i))
		r1 = r1.Add(r1, addend)

		if i%10 == 0 && time.Now().Sub(start) >= d {
			break
		}
	}
	return r1.FloatString(40)
}

var jsCalcHarmonic = js.FuncOf(func(this js.Value, args []js.Value) any {
	if len(args) != 1 {
		panic("want one argument")
	}

	s := calcHarmonic(args[0].Float())
	return js.ValueOf(s)
})
