//go:build js && wasm

package main

import (
	"syscall/js"
)

// Size of each cell in pixels (width and height)
const cellSize = 20

// Logical grid dimensions
const numRows = 25
const numCols = 25

// Period (in milliseconds) between re-draws when running
const runPeriod = 300

type gameState struct {
	canvas     js.Value
	runButton  js.Value
	stepButton js.Value

	// Game of life grid; true means the cell is alive.
	grid [numRows][numCols]bool

	running bool
}

func main() {
	doc := js.Global().Get("document")

	state := gameState{
		canvas:     doc.Call("getElementById", "gameCanvas"),
		runButton:  doc.Call("getElementById", "runButton"),
		stepButton: doc.Call("getElementById", "stepButton"),
	}
	state.initializeGrid()

	state.canvas.Call("addEventListener", "click", js.FuncOf(
		func(this js.Value, args []js.Value) any {
			x := args[0].Get("offsetX").Int()
			y := args[0].Get("offsetY").Int()
			state.onCanvasClicked(x, y)
			return nil
		}))
	state.runButton.Call("addEventListener", "click", js.FuncOf(
		func(this js.Value, args []js.Value) any {
			state.onRunClicked()
			return nil
		}))
	state.stepButton.Call("addEventListener", "click", js.FuncOf(
		func(this js.Value, args []js.Value) any {
			state.onStepClicked()
			return nil
		}))

	state.draw()

	select {}
}

func (state *gameState) initializeGrid() {
	// Draw a glider in the center of the grid
	state.grid[12][11] = true
	state.grid[12][12] = true
	state.grid[12][13] = true
	state.grid[11][13] = true
	state.grid[10][12] = true
}

func (state *gameState) onCanvasClicked(x, y int) {
	defer state.draw()
	r := y / cellSize
	c := x / cellSize

	state.grid[r][c] = !state.grid[r][c]
	state.draw()
}

func (state *gameState) onStepClicked() {
	defer state.draw()
	state.advance()
}

func (state *gameState) onRunClicked() {
	defer state.draw()
	if state.running {
		state.running = false
		state.runButton.Set("innerText", "Run")
		return
	}

	state.running = true
	state.runButton.Set("innerText", "Stop")

	js.Global().Call("setTimeout", js.FuncOf(func(this js.Value, args []js.Value) any {
		state.runningStep()
		return nil
	}), runPeriod)
}

func (state *gameState) runningStep() {
	if state.running {
		state.advance()
		state.draw()
		js.Global().Call("setTimeout", js.FuncOf(func(this js.Value, args []js.Value) any {
			state.runningStep()
			return nil
		}), runPeriod)
	}
}

func (state *gameState) draw() {
	cctx := state.canvas.Call("getContext", "2d")
	// Cells
	for r := range numRows {
		for c := range numCols {
			if state.grid[r][c] {
				cctx.Set("fillStyle", "#baba18")
			} else {
				cctx.Set("fillStyle", "#f4f4f4")
			}

			cctx.Call("fillRect", c*cellSize, r*cellSize, cellSize, cellSize)
		}
	}

	// Horizontal lines
	cctx.Set("strokeStyle", "#000000")
	for r := range numRows {
		cctx.Call("beginPath")
		cctx.Call("moveTo", 0, (r+1)*cellSize-1)
		cctx.Call("lineTo", numCols*cellSize, (r+1)*cellSize-1)
		cctx.Call("stroke")
	}

	// Vertical lines
	for c := range numCols {
		cctx.Call("beginPath")
		cctx.Call("moveTo", (c+1)*cellSize-1, 0)
		cctx.Call("lineTo", (c+1)*cellSize-1, numRows*cellSize)
		cctx.Call("stroke")
	}
}

// advance updates the game state to the next generation using game of life
// rules.
func (state *gameState) advance() {
	countNeighbors := func(r, c int) int {
		count := 0
		for dr := -1; dr <= 1; dr++ {
			for dc := -1; dc <= 1; dc++ {
				if dr == 0 && dc == 0 {
					continue
				}

				r2 := r + dr
				c2 := c + dc
				if r2 < 0 || r2 >= numRows || c2 < 0 || c2 >= numCols {
					continue
				}

				if state.grid[r2][c2] {
					count++
				}
			}
		}
		return count
	}

	newGrid := [numRows][numCols]bool{}
	for r := range numRows {
		for c := range numCols {
			neighbors := countNeighbors(r, c)
			if state.grid[r][c] {
				newGrid[r][c] = neighbors == 2 || neighbors == 3
			} else {
				newGrid[r][c] = neighbors == 3
			}
		}
	}

	state.grid = newGrid
}
