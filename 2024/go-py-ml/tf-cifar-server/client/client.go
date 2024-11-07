package main

import (
	"encoding/binary"
	"log"
	"net"
	"time"
)

func createPacket(ty int, msg string) []byte {
	msglen := uint32(len(msg)) + 1
	buf := make([]byte, msglen+4)
	binary.BigEndian.PutUint32(buf[0:], msglen)
	buf[4] = byte(ty)
	copy(buf[5:], msg)
	return buf
}

func main() {
	c, err := net.Dial("unix", "/tmp/imageserver.sock")
	if err != nil {
		panic(err)
	}
	defer c.Close()

	pc := createPacket(17, "hi there")
	_, err = c.Write(pc)
	if err != nil {
		log.Fatal("write error:", err)
	}
	time.Sleep(100 * time.Millisecond)
}
