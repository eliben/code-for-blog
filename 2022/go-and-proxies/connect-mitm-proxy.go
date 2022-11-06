package main

import (
	"bufio"
	"crypto"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"math/big"
	"net"
	"net/http"
	"net/url"
	"strings"
	"time"
)

func createCert(dnsNames []string, parent *x509.Certificate, parentKey crypto.PrivateKey, hoursValid int) (cert []byte, priv []byte) {
	log.Println("creating cert for domains:", dnsNames)
	privateKey, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		log.Fatalf("Failed to generate private key: %v", err)
	}

	serialNumberLimit := new(big.Int).Lsh(big.NewInt(1), 128)
	serialNumber, err := rand.Int(rand.Reader, serialNumberLimit)
	if err != nil {
		log.Fatalf("Failed to generate serial number: %v", err)
	}

	template := x509.Certificate{
		SerialNumber: serialNumber,
		Subject: pkix.Name{
			Organization: []string{"My Corp"},
		},
		DNSNames:  dnsNames,
		NotBefore: time.Now(),
		NotAfter:  time.Now().Add(time.Duration(hoursValid) * time.Hour),

		KeyUsage:              x509.KeyUsageDigitalSignature,
		ExtKeyUsage:           []x509.ExtKeyUsage{x509.ExtKeyUsageServerAuth, x509.ExtKeyUsageClientAuth},
		BasicConstraintsValid: true,
	}

	derBytes, err := x509.CreateCertificate(rand.Reader, &template, parent, &privateKey.PublicKey, parentKey)
	if err != nil {
		log.Fatalf("Failed to create certificate: %v", err)
	}
	pemCert := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: derBytes})
	if pemCert == nil {
		log.Fatal("failed to encode certificate to PEM")
	}

	privBytes, err := x509.MarshalPKCS8PrivateKey(privateKey)
	if err != nil {
		log.Fatalf("Unable to marshal private key: %v", err)
	}
	pemKey := pem.EncodeToMemory(&pem.Block{Type: "PRIVATE KEY", Bytes: privBytes})
	if pemCert == nil {
		log.Fatal("failed to encode key to PEM")
	}

	return pemCert, pemKey
}

func loadX509KeyPair(certFile, keyFile string) (cert *x509.Certificate, key any, err error) {
	cf, err := ioutil.ReadFile(certFile)
	if err != nil {
		return nil, nil, err
	}

	kf, err := ioutil.ReadFile(keyFile)
	if err != nil {
		return nil, nil, err
	}
	certBlock, _ := pem.Decode(cf)
	cert, err = x509.ParseCertificate(certBlock.Bytes)
	if err != nil {
		return nil, nil, err
	}

	keyBlock, _ := pem.Decode(kf)
	key, err = x509.ParsePKCS8PrivateKey(keyBlock.Bytes)
	if err != nil {
		return nil, nil, err
	}

	return cert, key, nil
}

type forwardProxy struct {
	caCert *x509.Certificate
	caKey  any
}

func createForwardProxy(caCertFile, caKeyFile string) *forwardProxy {
	caCert, caKey, err := loadX509KeyPair(caCertFile, caKeyFile)
	if err != nil {
		log.Fatal("Error loading CA certificate/key:", err)
	}
	log.Printf("loaded CA certificate and key; IsCA=%v\n", caCert.IsCA)

	return &forwardProxy{
		caCert: caCert,
		caKey:  caKey,
	}
}

func (p *forwardProxy) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	if req.Method == http.MethodConnect {
		p.proxyConnect(w, req)
	} else {
		http.Error(w, "this proxy only supports CONNECT", http.StatusMethodNotAllowed)
	}
}

func (p *forwardProxy) proxyConnect(w http.ResponseWriter, req *http.Request) {
	log.Printf("CONNECT requested to %v (from %v)", req.Host, req.RemoteAddr)

	hj, ok := w.(http.Hijacker)
	if !ok {
		log.Fatal("http server doesn't support hijacking connection")
	}

	clientConn, _, err := hj.Hijack()
	if err != nil {
		log.Fatal("http hijacking failed")
	}

	host, _, err := net.SplitHostPort(req.Host)
	if err != nil {
		log.Fatal("error splitting host/port:", err)
	}
	pemCert, pemKey := createCert([]string{host}, p.caCert, p.caKey, 240)
	tlsCert, err := tls.X509KeyPair(pemCert, pemKey)
	if err != nil {
		log.Fatal(err)
	}

	if _, err := clientConn.Write([]byte("HTTP/1.1 200 OK\r\n\r\n")); err != nil {
		log.Fatal("error writing status to client:", err)
	}

	tlsConfig := &tls.Config{
		PreferServerCipherSuites: true,
		CurvePreferences:         []tls.CurveID{tls.X25519, tls.CurveP256},
		MinVersion:               tls.VersionTLS13,
		Certificates:             []tls.Certificate{tlsCert},
	}
	if err != nil {
		log.Fatal(err)
	}

	tlsConn := tls.Server(clientConn, tlsConfig)
	defer tlsConn.Close()
	if err := tlsConn.Handshake(); err != nil {
		log.Fatal("error handshake")
	}

	connReader := bufio.NewReader(tlsConn)

	for {
		r, err := http.ReadRequest(connReader)
		if err == io.EOF {
			fmt.Println("at EOF")
			break
		} else if err != nil {
			log.Fatal(err)
		}

		targetClient := &http.Client{}
		r.RequestURI = ""
		r.URL = parseToUrl(req.Host)
		fmt.Printf("req: %+v\n", r)
		resp, err := targetClient.Do(r)
		if err != nil {
			log.Fatal("error client response:", err)
		}
		fmt.Println(resp)
		defer resp.Body.Close()

		if err := resp.Write(tlsConn); err != nil {
			log.Println("error writing response back:", err)
		}
	}
}

func parseToUrl(addr string) *url.URL {
	if !strings.HasPrefix(addr, "https") {
		addr = "https://" + addr
	}
	toUrl, err := url.Parse(addr)
	if err != nil {
		log.Fatal(err)
	}
	return toUrl
}

func main() {
	var addr = flag.String("addr", "127.0.0.1:9999", "proxy address")
	caCertFile := flag.String("cacertfile", "", "certificate .pem file for trusted CA")
	caKeyFile := flag.String("cakeyfile", "", "key .pem file for trusted CA")
	flag.Parse()

	proxy := createForwardProxy(*caCertFile, *caKeyFile)

	log.Println("Starting proxy server on", *addr)
	if err := http.ListenAndServe(*addr, proxy); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}
