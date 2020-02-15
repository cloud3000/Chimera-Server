<img  src="http://www.cloud3000.com/img/chimera_sm.png" alt="Chimera logo">


# Chimirror-Server
 **Chimirror** is a technical term for **Chimera**. The *Chimera* according to Greek mythology, was a hybrid beast of Asia Minor, composed of the parts of more than one animal.

Technically the **Chimirror-Server** is a hybrid technology, composed of the parts of more than one multi-generational computing paradigm:
  
  - The Main-Frame
  - The Unix Philosophy
  - The IoT (Internet of Things)

From a simple perspective it's just a multiplexer for Advanced Web Processing. However, it's much more, it's a philosophy based on mathematical theories of concurrency known as process algebras, or process calculi, based on message passing via channels. Using one bidirectional web socket, and three application process channels (stdout, stdin, and stderr).

~~~
    Client       |   Applications Process                 
                 |         
                 |         />>----->> stdin
    WebSocket <<- ->> ---<> <<-----<< stderr
                 |         \<<-----<< stdout
                 |
                 |
~~~
  
To make use of, or to criticize, you must have a good understanding of what a **Main-Frame** is, and how they were used in the 1960's, 70's, 80's, and still used today. You must also understand the **Unix Philosophy**, and the Internet of Things. Knowing the history is critical for understanding, if you're at a loss then try [Duck.com](https://www.Duck.com "The DuckDuckGo Search engine") Read about the **History** of the industry that you work in. 

Start with [Charles Babbage](https://www.youtube.com/watch?v=_mldLwKvT-M "Charles Babbage Biography") and 
[Ada Lovelace]( https://www.youtube.com/watch?v=IZptxisyVqQ "Ada Lovelace Biography").

---
### **How does it work?**
- One **Chimirror-Server** supports multiple web-clients and multiple applications processes. It is 
- 
  - A new application process is created for each new web client connection.
  + A web client is connected to the new application process on the server using a client web socket and the process's standard I/O (*stdin, stdout, and stderr*).
  + Communication designed to be whatever is agreed upon by the web client and the application process. For example you can agree to use **JSON** data structures where both client and server contain the algorithms to support the data structures and their purpose.
- Each web client and application process relationship is defined as a **session**.
- The number of sessions supported by 1 **Chimirror-Server** is limited to the hardware resources available. A small laptop should be able to support hundreds of sessions, whereas a large server might support hundreds of thousands.

It should be noted that each web client can be connected to only one applications process. The application process itself is not limited in anyway, it can create and manage it's own process tree. 

NOTE: When the application process ends, the session ends, and the websocket is disconnected. Likewise, when the web client ends the **Chimirror-Server** sends a EXIT signal to the application process, and the process should gracefully shutdown.

---
### Where can it be used?
It's important to note that the application process can be written in any programming language that supports standard I/O. This is difference than most application frameworks, like nodejs, express, and socket.io all require your application to be written in JavaScript, and likewise Flask requires Python, and so on. With **Chimirror-Server** you can use any language, at any scale. 
- Single User applications (using any programming language) for desktops, laptops, or tablets.
- Small business in-house servers.
- Large Cloud Server environments.
- The server is currently built for a unix environment.
- Web Clients run on any platform with a browser that supports web sockets.
- Cli Clients on any platform with a socket library.
  
---


I/O for each session process is handled as follows:
  - **stdin** is BLOCKED I/O, so the process will block when reading from stdin.
  - **stdout** is non-blocking, and will send events to the process manager, then read (as data),  forwarded to the browser.
  - **stderr** is handled the same as stdout, except the events are identified as error messages, forwarded to the browser.
