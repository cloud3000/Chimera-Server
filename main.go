package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
)

var addr = flag.String("addr", ":30747", "http service address")

func serveHome(w http.ResponseWriter, r *http.Request) {
	fmt.Println(r.URL)
	fmt.Printf("Received request from %v for Path: [%s]\n", r.RemoteAddr, r.URL.Path)

	if r.URL.Path != "/" {
		fmt.Printf("[%s] not found\n", r.URL.Path)
		http.Error(w, "Not found", 404)
		return
	}
	if r.Method != "GET" {
		http.Error(w, "Method not allowed", 405)
		return
	}
	http.ServeFile(w, r, "index.html")
}

func main() {
	flag.Parse()
	hub := newHub()
	go hub.run()
	http.HandleFunc("/", serveHome)
	http.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		clientWs(hub, w, r)
	})

	log.Println(*addr)
	err := http.ListenAndServe(*addr, nil)
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}
