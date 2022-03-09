// Plots Benford distribution of first digit taken from a CSV data set.
// Data in CSV file from
// https://www.dof.ca.gov/Forecasting/Demographics/Estimates/e-1/
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/csv"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"

	"gonum.org/v1/plot"
	"gonum.org/v1/plot/plotter"
	"gonum.org/v1/plot/plotutil"
	"gonum.org/v1/plot/vg"
)

func main() {
	csvFile := flag.String("csvfile", "", "csv file to process")
	digitOut := flag.String("outdigits", "digits.png", "name of output PNG file for digit distribution")
	flag.Parse()

	f, err := os.Open(*csvFile)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	// Raw data: populations as strings
	var data []string

	r := csv.NewReader(f)
	for {
		rec, err := r.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}
		data = append(data, rec[3])
	}

	emitDigitPlots(*digitOut, data)
}

func emitDigitPlots(outFile string, populations []string) {
	digitMap := make(map[int]int)
	for _, d := range populations {
		firstDigit := d[0]
		digitMap[int(firstDigit)-int('0')]++
	}

	var vals []float64
	var names []string
	for i := 1; i <= 9; i++ {
		vals = append(vals, float64(digitMap[i])/float64(len(populations)))
		names = append(names, strconv.Itoa(i))
	}

	p := plot.New()
	p.Title.Text = "Digit distribution"
	p.Y.Label.Text = "P(d)"
	p.Y.Tick.Marker = digitTicks{}
	p.Y.Max = 0.4
	p.X.Label.Text = "First digit"

	bar, err := plotter.NewBarChart(plotter.Values(vals), vg.Points(20))
	if err != nil {
		log.Fatal(err)
	}
	bar.LineStyle.Width = vg.Length(0)
	bar.Color = plotutil.Color(0)

	p.Add(bar)
	p.NominalX(names...)

	if err := p.Save(5*vg.Inch, 3*vg.Inch, outFile); err != nil {
		log.Fatal(err)
	}
	fmt.Println("Wrote", outFile)
}

type digitTicks struct{}

func (digitTicks) Ticks(min, max float64) []plot.Tick {
	return []plot.Tick{
		plot.Tick{0, "0.0"},
		plot.Tick{0.2, "0.2"},
		plot.Tick{0.4, "0.4"},
		plot.Tick{0.3, "0.3"},
		plot.Tick{0.1, "0.1"},
		plot.Tick{0.15, ""},
		plot.Tick{0.25, ""},
		plot.Tick{0.35, ""},
		plot.Tick{0.05, ""},
	}
}
