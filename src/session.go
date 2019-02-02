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
)

const endoffile = "EOF"

func recvStdin(stdin io.WriteCloser, in <-chan []byte, sKey []string) {
	fmt.Println("session.recvStdin: started")
	for {
		select {
		// Accept input from the browser
		case message, ok := <-in:
			if ok {
				fmt.Printf("session.recvStdin: %s\n", sKey)
				// Write message to stdin of child
				stdin.Write([]byte(fmt.Sprintf("%s\n", message)))
			} else {
				fmt.Printf("recvStdin: Exiting %s\n", sKey)
				stdin.Write([]byte(fmt.Sprintf("exit\n")))
				return
			}
		}
	}
}

// Read the results from the process
func chanelStdout(out io.ReadCloser, client chan<- []byte, sKey []string) {
	fmt.Println("session.chanelStdout: started")
	buf := bufio.NewReader(out)
	for run := true; run; {
		fmt.Printf("session.chanelStdout: waiting on data from stdout %s\n", sKey)
		result, _, err := buf.ReadLine()
		if err != nil {
			if err.Error() != endoffile {
				fmt.Println(err)
			}
			run = false
		}
		test := string(result)
		fmt.Println("FINAL RETURN ", test)
		if test == "exiting" {
			run = false
		}
		if run {
			fmt.Printf("session.chanelStdout: data received from stdout %s\n", sKey)
			client <- []byte(fmt.Sprintf("stdout| %s", string(result)))
			fmt.Printf("session.chanelStdout: client <- %s\n", string(result))
		}
	}
	fmt.Printf("chanelStdout: Exiting %s\n", sKey)
}

func chanelStderr(out io.ReadCloser, client chan<- []byte, sKey []string) {
	fmt.Println("session.chanelStderr: started")
	buf := bufio.NewReader(out)
	for run := true; run; {
		fmt.Printf("session.chanelStderr: waiting on data from stderr %s\n", sKey)
		result, _, err := buf.ReadLine()
		if err != nil {
			if err.Error() != endoffile {
				fmt.Println(err)
			}
			run = false
		}
		if run {
			fmt.Printf("session.chanelStderr: data received from stderr %s\n", sKey)
			client <- []byte(fmt.Sprintf("stderr| %s", string(result)))
			fmt.Printf("session.chanelStderr: client <- %s\n", string(result))
		}
	}
	fmt.Printf("chanelStderr: Exiting %s\n", sKey)
}

func createProcess(hub *Hub, c *Client, command string) error {
	fmt.Println("createProcess: Starting process")
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

	c.conn.Close()
	fmt.Println("createProcess: Exiting process")
	return exiterr
}
