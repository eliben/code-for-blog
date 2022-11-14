// Example from ssh package's docs to execute simple remote command
// Fixed to actually work with known_hosts callback, etc.
package main

import (
	"flag"
	"log"
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
	flag.Parse()

	config := createSshConfig(*username, *keyFile)

	client, err := ssh.Dial("tcp", *addr, config)
	if err != nil {
		log.Fatal("Failed to dial: ", err)
	}
	defer client.Close()

	// Each ClientConn can support multiple interactive sessions,
	// represented by a Session.
	session, err := client.NewSession()
	if err != nil {
		log.Fatal("Failed to create session: ", err)
	}
	defer session.Close()

	// Once a Session is created, you can a single command on
	// the remote side using the Run method.
	session.Stdout = os.Stdout
	if err := session.Run("uname -a"); err != nil {
		log.Fatal("Failed to run: " + err.Error())
	}
}
