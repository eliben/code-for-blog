//go:build js && wasm

package main

import (
	"log"
	"math/big"
	"strconv"
	"syscall/js"
	"time"
)

func main() {
	doc := js.Global().Get("document")
	buttonElement := doc.Call("getElementById", "submitButton")
	inputElement := doc.Call("getElementById", "timeInput")
	outputElement := doc.Call("getElementById", "outputDiv")

	buttonElement.Call("addEventListener", "click", js.FuncOf(
		func(this js.Value, args []js.Value) any {
			input := inputElement.Get("value")
			inputFloat, err := strconv.ParseFloat(input.String(), 64)
			if err != nil {
				log.Println(err)
				return nil
			}
			s := calcHarmonic(inputFloat)
			outputElement.Set("innerText", s)
			return nil
		}))

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
