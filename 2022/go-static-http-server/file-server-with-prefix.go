package main

import "net/http"

func main() {
	port := ":9999"
	handler := http.StripPrefix("/static/", http.FileServer(http.Dir("files")))
	//handler := http.FileServer(http.Dir("files"))
	http.Handle("/static/", handler)
	http.ListenAndServe(port, nil)
}
