// Benford distribution of P(d=0) where d is the leading digit of a number,
// on a logarithmic X scale.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"fmt"
	"image/color"
	"log"

	"gonum.org/v1/plot"
	"gonum.org/v1/plot/plotter"
	"gonum.org/v1/plot/vg"
)

// getLeadDigit finds the first digit of n
func getLeadDigit(n int) int {
	for n >= 10 {
		n /= 10
	}
	return n
}

func main() {
	pngOut := flag.String("pngout", "digit1p.png", "name of output PNG file")
	flag.Parse()

	leadDigits := make(map[int]int)

	var data plotter.XYs
	var avgData plotter.XYs
	var psum float64

	// Need to be careful with the ranges and plot size here: some gonum bug
	// affects how many points are emitted in some cases.
	for i := 1; i < 11000; i++ {
		leadDigits[getLeadDigit(i)]++

		// p is the ratio of numbers starting with 1 seen up to and including i.
		p := float64(leadDigits[1]) / float64(i)
		psum += p

		data = append(data, plotter.XY{X: float64(i), Y: p})
		avgData = append(avgData, plotter.XY{X: float64(i), Y: psum / float64(i)})

		fmt.Printf("%5d: p=%f, avgp=%f\n", i, p, psum/float64(i))
	}

	// Now plot it
	p := plot.New()

	p.Title.Text = "d = first digit of N"
	p.X.Label.Text = "Up to N"
	p.Y.Label.Text = "P(d=1)"
	p.X.Scale = plot.LogScale{}
	p.X.Tick.Marker = myTicks{}

	lp, err := plotter.NewLine(data)
	if err != nil {
		log.Fatal(err)
	}
	lp.LineStyle.Width = vg.Points(1)
	lp.LineStyle.Color = color.RGBA{B: 255, A: 255}

	lpa, err := plotter.NewLine(avgData)
	if err != nil {
		log.Fatal(err)
	}
	lpa.LineStyle.Width = vg.Points(1)
	lpa.LineStyle.Color = color.RGBA{R: 255, B: 140, A: 255}

	//p.Add(lp, lpa)
	p.Add(lp)
	if err := p.Save(6*vg.Inch, 3*vg.Inch, *pngOut); err != nil {
		log.Fatal(err)
	}
	fmt.Println("Wrote", *pngOut)
}

type myTicks struct{}

func (myTicks) Ticks(min, max float64) []plot.Tick {
	return []plot.Tick{
		plot.Tick{1, "1"},
		plot.Tick{10, "10"},
		plot.Tick{20, "20"},
		plot.Tick{100, "100"},
		plot.Tick{200, "200"},
		plot.Tick{1000, "1000"},
		plot.Tick{1000, "1000"},
		plot.Tick{2000, "2000"},
		plot.Tick{10000, "10000"},
	}
}
