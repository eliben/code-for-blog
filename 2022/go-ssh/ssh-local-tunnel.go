// Following instructions from https://sosedoff.com/2015/05/25/ssh-port-forwarding-with-go.html
// significantly revamped for new Go versions, using knownhosts, etc.
// Usage example: local psql for remote postgres DB

package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"path/filepath"

	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"
)

func sshConfigPath(filename string) string {
	return filepath.Join(os.Getenv("HOME"), ".ssh", filename)
}

func createSshConfig(username, keyFile string) *ssh.ClientConfig {
	knownHostsCallback, err := knownhosts.New(sshConfigPath("known_hosts"))
	if err != nil {
		log.Fatal(err)
	}

	key, err := os.ReadFile(keyFile)
	if err != nil {
		log.Fatalf("unable to read private key: %v", err)
	}

	// Create the Signer for this private key.
	signer, err := ssh.ParsePrivateKey(key)
	if err != nil {
		log.Fatalf("unable to parse private key: %v", err)
	}

	// An SSH client is represented with a ClientConn.
	//
	// To authenticate with the remote server you must pass at least one
	// implementation of AuthMethod via the Auth field in ClientConfig,
	// and provide a HostKeyCallback.
	return &ssh.ClientConfig{
		User: username,
		Auth: []ssh.AuthMethod{
			ssh.PublicKeys(signer),
		},
		HostKeyCallback:   knownHostsCallback,
		HostKeyAlgorithms: []string{ssh.KeyAlgoED25519},
	}
}

func main() {
	addr := flag.String("addr", "", "ssh server address to dial as <hostname>:<port>")
	username := flag.String("user", "", "username for ssh")
	keyFile := flag.String("keyfile", "", "file with private key for SSH authentication")
	remotePort := flag.String("rport", "", "remote port for tunnel")
	localPort := flag.String("lport", "", "local port for tunnel")
	flag.Parse()

	config := createSshConfig(*username, *keyFile)

	client, err := ssh.Dial("tcp", *addr, config)
	if err != nil {
		log.Fatal("Failed to dial: ", err)
	}
	defer client.Close()

	listener, err := net.Listen("tcp", "localhost:"+*localPort)
	if err != nil {
		log.Fatal(err)
	}
	defer listener.Close()

	for {
		// Like ssh -L by default, local connections are handled one at a time.
		// While one local connection is active in runTunnel, others will be stuck
		// dialing, waiting for this Accept.
		local, err := listener.Accept()
		if err != nil {
			log.Fatal(err)
		}

		// Issue a dial to the remote server on our SSH client; here "localhost"
		// refers to the remote server.
		remote, err := client.Dial("tcp", "localhost:"+*remotePort)
		if err != nil {
			log.Fatal(err)
		}

		fmt.Println("tunnel established with", local.LocalAddr())
		runTunnel(local, remote)
	}
}

// runTunnel runs a tunnel between two connections; as soon as one connection
// reaches EOF or reports an error, both connections are closed and this
// function returns.
func runTunnel(local, remote net.Conn) {
	defer local.Close()
	defer remote.Close()
	done := make(chan struct{}, 2)

	go func() {
		io.Copy(local, remote)
		done <- struct{}{}
	}()

	go func() {
		io.Copy(remote, local)
		done <- struct{}{}
	}()

	<-done
}
