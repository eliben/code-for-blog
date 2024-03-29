// Generating SVGs for the blog post.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"io"
	"log"
	"math/rand"
	"os"
	"time"

	"github.com/eliben/go-sudoku"
	"github.com/eliben/go-sudoku/svg"
)

func DisplaySmallSVG(w io.Writer, values sudoku.Values) {
	startX := 10
	startY := 10
	cellsize := 40
	width := cellsize*9 + 20
	height := cellsize*9 + 20
	canvas := svg.New(w, width, height)

	for sq, d := range values {
		col := sq % 9
		x := startX + col*cellsize

		row := sq / 9
		y := startY + row*cellsize

		canvas.Rect(x, y, cellsize, cellsize, "stroke:black; stroke-width:2; fill:white")
		if d.Size() == 1 {
			canvas.Text(x+cellsize/2, y+cellsize/2, d.String(), "text-anchor:middle; dominant-baseline:middle; font-family:Helvetica; font-size:18px; fill:black")
		}
	}

	// Wider squares around 3x3 blocks
	for br := 0; br < 3; br++ {
		for bc := 0; bc < 3; bc++ {
			canvas.Rect(startX+bc*cellsize*3, startY+br*cellsize*3, cellsize*3, cellsize*3, "stroke:black; stroke-width:5; fill-opacity:0.0")
		}
	}

	canvas.End()
}

func svgToFile(values sudoku.Values, filename string) {
	rand.Seed(time.Now().UnixNano())
	f, err := os.Create(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	DisplaySmallSVG(f, values)
}

func main() {
	empty := sudoku.EmptyBoard()
	svgToFile(empty, "sudoku-empty.svg")

	board, solved := sudoku.Solve(empty, sudoku.SolveOptions{Randomize: true})
	if !solved {
		log.Fatal("could not solve")
	}
	svgToFile(board, "sudoku-solved.svg")

	brd := sudoku.GenerateSymmetrical(22)
	fmt.Println(sudoku.CountHints(brd))
	svgToFile(brd, "sudoku-puzzle.svg")
}
