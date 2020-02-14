// Copyright 2017 The Advanced Terminal Processor Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"unicode/utf8"
)

const endoffile = "EOF"

func fixUtf(r rune) rune {
	if r == utf8.RuneError {
		return rune('.')
	}
	return r
}

func clientWrite(out chan<- []byte, data []byte) (err error) {

	defer func() {
		// recover from panic, if channel is closed.
		if r := recover(); r != nil {
			err = fmt.Errorf("%v", r)
			fmt.Printf("write: error writing %s on channel: %v\n", data, err)
			return
		}

	}()

	out <- data

	return err
}

func recvStdin(stdin io.WriteCloser, in <-chan []byte, sKey []string) {
	//fmt.Println("session.recvStdin: started")
	for {
		select {
		// Accept input from the browser
		case message, ok := <-in:
			if ok {
				//fmt.Printf("session.recvStdin: %s\n", sKey)
				// Write message to stdin of child
				stdin.Write([]byte(fmt.Sprintf("%s\n", message)))
				//fmt.Printf("sent to stdin:%s", message)
			} else {
				//fmt.Printf("recvStdin: Exiting %s\n", sKey)
				stdin.Write([]byte(fmt.Sprintf("%x\n", "recvStdin error 1")))
				return
			}
		}
	}
}

// chanelStdout will read data, stdout from the child process.
// This is BLOCKED I/O, it will continuously read bytes,
// and then BLOCK until more bytes are sent from the child process.
func chanelStdout(out io.ReadCloser, client chan<- []byte, sKey []string) {
	//p := fmt.Printf
	//fmt.Println("session.chanelStdout: started")
	buf := bufio.NewReader(out)
	for run := true; run; {
		//p("session.chanelStdout: BLOCKED I/O, waiting on data from stdout %s\n", sKey)
		result, _, err := buf.ReadLine()
		if err != nil {
			// if err.Error() != endoffile {
			// 	fmt.Println(err)
			// }
			fmt.Println(err)
			run = false
		}
		//t := time.Now()
		payload := strings.Map(fixUtf, string(result))
		if payload == "exiting" {
			fmt.Println("session child process says bye ", payload)
			run = false
		}

		if run {
			//p("%d-%02d-%02d %02d:%02d:%02d session.chanelStdout: data received from stdout %s\n",
			//	t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second(), payload)
			//client <- []byte(fmt.Sprintf("stdout| %s", payload))

			clientWrite(client, []byte(fmt.Sprintf("stdout| %s", payload)))
			fmt.Printf("session.chanelStdout recv from child: %s\n", string(payload))

			//p("%d-%02d-%02d %02d:%02d:%02d session.chanelStdout: Sent to client <- chan\n",
			//	t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
		}
	}
	fmt.Printf("chanelStdout: Function Return %s\n", sKey)
}

// chanelStderr will read data, stderr from the child process.
// This is BLOCKED I/O, it will continuously read bytes,
// and then BLOCK until more bytes are sent from the child process.
func chanelStderr(out io.ReadCloser, client chan<- []byte, sKey []string) {
	//fmt.Println("session.chanelStderr: started")
	buf := bufio.NewReader(out)
	for run := true; run; {
		//fmt.Printf("session.chanelStderr: waiting on data from stderr %s\n", sKey)
		result, _, err := buf.ReadLine()
		if err != nil {
			// if err.Error() != endoffile {
			// 	fmt.Println(err)
			// }
			fmt.Println(err)
			run = false
		}
		if run {
			//fmt.Printf("session.chanelStderr: data received from stderr %s\n", sKey)
			//client <- []byte(fmt.Sprintf("stderr| %s", string(result)))
			clientWrite(client, []byte(fmt.Sprintf("stderr| %s", string(result))))
			fmt.Printf("session.chanelStderr recv from child: %s\n", string(result))
		}
	}
	fmt.Printf("chanelStderr: Function Return %s\n", sKey)
}

func createProcess(hub *Hub, c *Client, command string) error {
	//fmt.Println("createProcess: Starting process")
	cmd := exec.Command(command, fmt.Sprintf("%s", c.socketkey))

	in, err := cmd.StdinPipe()
	if err != nil {
		fmt.Printf("Error creating stdin pipe\n%s\n", err.Error())
		os.Exit(1)
	}

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Printf("Error creating stdout pipe\n%s\n", err.Error())
		os.Exit(1)
	}

	stderr, err := cmd.StderrPipe()
	if err != nil {
		fmt.Printf("Error creating stderr pipe\n%s\n", err.Error())
		os.Exit(1)
	}

	defer func() {
		in.Close()
		stdout.Close()
		stderr.Close()
	}()

	// Start the process
	if err = cmd.Start(); err != nil {
		panic(err)
	}

	go recvStdin(in, c.stdInTochild, c.socketkey)
	go chanelStdout(stdout, c.send, c.socketkey)
	go chanelStderr(stderr, c.send, c.socketkey)
	// go formIO(c.stdInTochild, c.send, c.socketkey)

	exiterr := cmd.Wait()
	if exiterr != nil {
		fmt.Println(exiterr.Error())
	}

	//c.conn.Close()
	fmt.Println("createProcess: Exiting, END OF SESSION")
	return exiterr
}
