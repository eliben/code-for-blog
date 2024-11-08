package main

import (
	"encoding/binary"
	"flag"
	"fmt"
	"image/png"
	"log"
	"net"
	"os"
	"time"
)

const messageTypeEcho = 0
const messageTypeClassify = 1

// TODO: add function comments here

// sendPacket sends an arbitrary data packet to the server using the
// length-delimited protocol described in the README. ty is the type byte.
func sendPacket(c net.Conn, ty int, body []byte) {
	msglen := uint32(len(body)) + 1
	buf := make([]byte, msglen+4)
	binary.BigEndian.PutUint32(buf[0:], msglen)
	buf[4] = byte(ty)
	copy(buf[5:], body)

	if _, err := c.Write(buf); err != nil {
		log.Fatal("write error:", err)
	}
}

// readPacket reads a packet from c and returns the type byte and the body.
func readPacket(c net.Conn) (int, []byte) {
	var msglen uint32
	err := binary.Read(c, binary.BigEndian, &msglen)
	if err != nil {
		log.Fatal("binary.Read failed:", err)
	}
	buf := make([]byte, msglen)
	_, err = c.Read(buf)
	if err != nil {
		log.Fatal("read error:", err)
	}
	return int(buf[0]), buf[1:]
}

func runBenchmark(c net.Conn, numIters int) {
	// Create a []byte with 3072 bytes.
	body := make([]byte, 3072)
	for i := range body {
		body[i] = byte(i % 256)
	}

	t1 := time.Now()
	for range numIters {
		sendPacket(c, messageTypeEcho, body)
		cmd, resp := readPacket(c)
		if cmd != 0 || len(resp) != len(body) {
			log.Fatal("bad response")
		}
	}
	elapsed := time.Since(t1)
	fmt.Printf("Num packets: %d, Elapsed time: %s\n", numIters, elapsed)
	fmt.Printf("Average time per request: %d ns\n", elapsed.Nanoseconds()/int64(numIters))
}

func classify(c net.Conn, imgPath string) {
	// Load the image from the file.
	f, err := os.Open(imgPath)
	if err != nil {
		log.Fatal("open error:", err)
	}
	defer f.Close()

	img, err := png.Decode(f)
	if err != nil {
		log.Fatal("decode error:", err)
	}

	// Convert the image to a byte slice, 32x32x3 where eaach color channel
	// is in row-major. Red first, then green, then blue.
	imgBytes := make([]byte, 32*32*3)
	for y := range 32 {
		for x := range 32 {
			idx := y*32 + x
			r, g, b, _ := img.At(x, y).RGBA()
			imgBytes[idx] = byte(r / 256)
			imgBytes[1024+idx] = byte(g / 256)
			imgBytes[2048+idx] = byte(b / 256)
		}
	}

	t1 := time.Now()
	sendPacket(c, messageTypeClassify, imgBytes)
	cmd, resp := readPacket(c)
	elapsed := time.Since(t1)
	fmt.Printf("Response cmd=%d, class=%s (elapsed time: %v)\n", cmd, string(resp), elapsed)
}

func main() {
	// Flags:
	// -measure <n>: measure the time to send <n> packets
	// -classify <image-file>: classify the image in <image-file>

	measureFlag := flag.Int("measure", 0, "measure the time to send <n> packets")
	classifyFlag := flag.String("classify", "", "classify the image in <image-file>")
	flag.Parse()

	c, err := net.Dial("unix", "/tmp/imageserver.sock")
	if err != nil {
		panic(err)
	}
	defer c.Close()

	if *measureFlag > 0 {
		runBenchmark(c, *measureFlag)
	} else if len(*classifyFlag) > 0 {
		classify(c, *classifyFlag)
	} else {
		log.Fatal("no command specified")
	}
}
