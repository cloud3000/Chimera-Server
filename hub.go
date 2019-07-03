// Copyright 2017 The Advanced Terminal Processor Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
package main

import "fmt"

// Hub maintains the set of active clients and broadcasts messages to the
// clients.
type Hub struct {
	// Registered clients.
	clients map[*Client]bool

	// Inbound messages from the clients.
	broadcast chan []byte

	// Register requests from the clients.
	register chan *Client

	// Unregister requests from clients.
	unregister chan *Client
}

func newHub() *Hub {
	return &Hub{
		broadcast:  make(chan []byte),
		register:   make(chan *Client),
		unregister: make(chan *Client),
		clients:    make(map[*Client]bool),
	}
}

func (h *Hub) run() {
	fmt.Println("hub.run: started")
	for {
		select {
		case client := <-h.register:
			fmt.Printf("hub.run: ********* BEGIN NEW CLIENT: %v\n", client.hub.clients)
			h.clients[client] = true
		case client := <-h.unregister:
			fmt.Printf("hub.run: ********* END CLIENT:\n")
			if _, ok := h.clients[client]; ok {
				// client.stdInTochild <- []byte("exit")
				delete(h.clients, client)
				close(client.send)
				close(client.stdErrFromchild)
				close(client.stdInTochild)
				close(client.stdOutFromchild)
			}
		}
	}
}
