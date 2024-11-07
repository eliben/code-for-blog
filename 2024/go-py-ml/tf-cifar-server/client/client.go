package main

import (
	"encoding/binary"
	"fmt"
	"log"
	"net"
)

func createPacket(ty int, msg string) []byte {
	msglen := uint32(len(msg)) + 1
	buf := make([]byte, msglen+4)
	binary.BigEndian.PutUint32(buf[0:], msglen)
	buf[4] = byte(ty)
	copy(buf[5:], msg)
	return buf
}

func readPacket(c net.Conn) (int, string) {
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
	return int(buf[0]), string(buf[1:])
}

func main() {
	c, err := net.Dial("unix", "/tmp/imageserver.sock")
	if err != nil {
		panic(err)
	}
	defer c.Close()

	pc := createPacket(0, "hi there")
	_, err = c.Write(pc)
	if err != nil {
		log.Fatal("write error:", err)
	}

	// TODO: for benchmarking, send 3072 random bytes and read them back.

	fmt.Println(readPacket(c))
}
