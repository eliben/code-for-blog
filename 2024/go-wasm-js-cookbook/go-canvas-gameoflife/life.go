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

type gameState struct {
	canvas     js.Value
	runButton  js.Value
	stepButton js.Value

	// Grid of cells
	grid [numRows][numCols]bool
}

func main() {
	doc := js.Global().Get("document")

	state := gameState{
		canvas:     doc.Call("getElementById", "gameCanvas"),
		runButton:  doc.Call("getElementById", "runButton"),
		stepButton: doc.Call("getElementById", "stepButton"),
	}
	state.initialize()

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
			state.onRunClicked()
			return nil
		}))

	state.draw()

	select {}
}

func (state *gameState) initialize() {
	// Draw a glider in the center of the grid
	state.grid[12][11] = true
	state.grid[12][12] = true
	state.grid[12][13] = true
	state.grid[11][13] = true
	state.grid[10][12] = true
}

func (state *gameState) onCanvasClicked(x, y int) {
	r := y / cellSize
	c := x / cellSize

	state.grid[r][c] = !state.grid[r][c]
	state.draw()
}

func (state *gameState) onStepClicked() {
}

func (state *gameState) onRunClicked() {
}

// updateStep updates single step of game of life rules.
func (state *gameState) updateStep() {
	// TODO: Implement game of life rules
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
