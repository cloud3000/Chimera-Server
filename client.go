// Copyright 2017 The Advanced Terminal Processor Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/websocket"
)

const (
	// Time allowed to write a message to the peer.
	writeWait = 10 * time.Second

	// Time allowed to read the next pong message from the peer.
	pongWait = 60 * time.Second

	// Send pings to peer with this period. Must be less than pongWait.
	pingPeriod = (pongWait * 9) / 10

	// Maximum message size allowed from peer.
	maxMessageSize = 4096

	// The service created for the client.
	clientService = "/myhome/michael/Chimirror-Server/animal"
)

var (
	newline = []byte{'\n'}
	space   = []byte{' '}
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  65536,
	WriteBufferSize: 65536,
}

// Client maintains communication between the websocket connection and the hub.
type Client struct {
	hub *Hub

	// The websocket connection.
	conn *websocket.Conn

	// Buffered channel of outbound messages.
	send chan []byte

	// Buffered channels of child I/O.
	stdInTochild    chan []byte
	stdErrFromchild chan string
	stdOutFromchild chan string
	closeChild      chan bool

	// Unique Client Data
	socketkey    []string
	socketOrigin []string
	socketaddr   string
	socketproto  string
}

// readPump pumps messages from the websocket connection to the hub.
//
// The application runs readPump in a per-connection goroutine. The application
// ensures that there is at most one reader on a connection by executing all
// reads from this goroutine.
func (c *Client) readPump() {
	defer func() {
		c.hub.unregister <- c
	}()
	c.conn.SetReadLimit(maxMessageSize)
	c.conn.SetReadDeadline(time.Now().Add(pongWait))
	c.conn.SetPongHandler(func(string) error { c.conn.SetReadDeadline(time.Now().Add(pongWait)); return nil })
	//fmt.Println("readPump: starting infinant loop")
	for {
		//fmt.Println("readPump: wait for messages from c.conn.ReadMessage()")
		_, message, err := c.conn.ReadMessage()
		if err != nil {
			//fmt.Println("readPump: websocket.IsUnexpectedCloseError")
			if websocket.IsUnexpectedCloseError(err, websocket.CloseGoingAway) {
				log.Printf("error: %v", err)
			}
			break
		}
		message = bytes.TrimSpace(bytes.Replace(message, newline, space, -1))
		fmt.Printf("readPump: Sending to child: [%v]\n", string(message))
		c.stdInTochild <- message
	}
}

// writePump sends messages from the child to the client browser.
//
func (c *Client) writePump() {
	ticker := time.NewTicker(pingPeriod)
	defer func() {
		ticker.Stop()
		c.conn.Close()
	}()
	for {
		select {
		case message, ok := <-c.send:
			c.conn.SetWriteDeadline(time.Now().Add(writeWait))
			if !ok {
				fmt.Println("writePump: Sending websocket.CloseMessage to the client browser.")
				c.conn.WriteMessage(websocket.CloseMessage, []byte{})
				return
			}
			//fmt.Println("writePump: c.conn.NextWriter(websocket.TextMessage)")
			w, err := c.conn.NextWriter(websocket.TextMessage)
			if err != nil {
				fmt.Printf("ERROR NextWriter: %v\n", err)
				return
			}
			//fmt.Println("========================================================")
			fmt.Printf("writePump: sending to client, %s\n", string(message))
			w.Write(message)

			// Add queued messages to the current websocket message.
			// n := len(c.send)
			// for i := 0; i < n; i++ {
			// 	w.Write(newline)
			// 	w.Write(<-c.send)
			// }

			if err := w.Close(); err != nil {
				return
			}
		case <-ticker.C:
			//fmt.Println("writePump: case <-ticker.C:")
			c.conn.SetWriteDeadline(time.Now().Add(writeWait))
			if err := c.conn.WriteMessage(websocket.PingMessage, []byte{}); err != nil {
				return
			}
		}
	}
}

// clientWs handles websocket requests from the peer.
func clientWs(hub *Hub, w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return
	}

	fmt.Println("clientWs: setting up new client")
	client := &Client{
		hub:             hub,
		conn:            conn,
		send:            make(chan []byte, 65536),
		stdInTochild:    make(chan []byte, 65536),
		stdErrFromchild: make(chan string, 65536),
		stdOutFromchild: make(chan string, 65536),
		closeChild:      make(chan bool),
		socketkey:       []string(r.Header["Sec-Websocket-Key"]),
		socketOrigin:    []string(r.Header["Origin"]),
		socketaddr:      string(r.RemoteAddr),
		socketproto:     string(r.Proto),
	}

	client.hub.register <- client
	go client.writePump()
	go createProcess(hub, client, clientService)
	fmt.Println("clientWs: client.readPump(), is listening for data from the client web browser")
	client.readPump()
	fmt.Println("clientWs: client.readPump(), stopped listening to the client web browser")
}
