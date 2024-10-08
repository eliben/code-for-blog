//go:build js && wasm

package main

import (
	"encoding/json"
	"fmt"
	"log"
	"syscall/js"
)

const wsServerAddress = "ws://127.0.0.1:4050"

type Event struct {
	X int `json:"x"`
	Y int `json:"y"`
}

func main() {
	log.Println("wasm loaded")

	doc := js.Global().Get("document")
	tickerElement := doc.Call("getElementById", "timeticker")
	boxElement := doc.Call("getElementById", "box")
	outputElement := doc.Call("getElementById", "output")

	wsCtor := js.Global().Get("WebSocket")
	wsEcho := wsCtor.New(wsServerAddress + "/wsecho")
	wsTime := wsCtor.New(wsServerAddress + "/wstime")

	wsTime.Call("addEventListener", "message", js.FuncOf(
		func(this js.Value, args []js.Value) any {
			msg := args[0].Get("data").String()
			tickerElement.Set("innerText", msg)
			return nil
		}))

	boxElement.Call("addEventListener", "mousemove", js.FuncOf(
		func(this js.Value, args []js.Value) any {
			event := args[0]
			wsSend(wsEcho, Event{
				X: event.Get("clientX").Int(),
				Y: event.Get("clientY").Int(),
			})
			return nil
		}))

	wsEcho.Call("addEventListener", "message", js.FuncOf(
		func(this js.Value, args []js.Value) any {
			msg := []byte(args[0].Get("data").String())
			var ev Event
			if err := json.Unmarshal(msg, &ev); err != nil {
				log.Fatal(err)
			}
			coordMsg := fmt.Sprintf("Coordinates: (%v, %v)", ev.X, ev.Y)
			outputElement.Set("innerText", coordMsg)
			return nil
		}))

	select {}
}

func wsSend(sock js.Value, msg any) {
	if !sock.IsNull() || sock.Get("readyState").Equal(js.Global().Get("WebSocket").Get("OPEN")) {
		b, err := json.Marshal(msg)
		if err != nil {
			log.Fatal(err)
		}
		sock.Call("send", string(b))
	} else {
		log.Println("socket is not open")
	}
}
