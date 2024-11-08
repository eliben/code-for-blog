package main

import (
	"encoding/binary"
	"flag"
	"fmt"
	"log"
	"net"
	"time"
)

func createPacket(ty int, body []byte) []byte {
	msglen := uint32(len(body)) + 1
	buf := make([]byte, msglen+4)
	binary.BigEndian.PutUint32(buf[0:], msglen)
	buf[4] = byte(ty)
	copy(buf[5:], body)
	return buf
}

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
		// Create a []byte with 3072 bytes.
		body := make([]byte, 3072)
		for i := range body {
			body[i] = byte(i % 256)
		}

		t1 := time.Now()
		for range *measureFlag {
			pc := createPacket(0, body)
			_, err = c.Write(pc)
			if err != nil {
				log.Fatal("write error:", err)
			}
			cmd, resp := readPacket(c)
			if cmd != 0 || len(resp) != len(body) {
				log.Fatal("bad response")
			}
		}
		elapsed := time.Since(t1)
		fmt.Printf("Num packets: %d, Elapsed time: %s\n", *measureFlag, elapsed)
		fmt.Printf("Average time per request: %d ns\n", elapsed.Nanoseconds()/int64(*measureFlag))
	} else if len(*classifyFlag) > 0 {

	} else {
		log.Fatal("no command specified")
	}
}
