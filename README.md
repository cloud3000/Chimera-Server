<img  src="http://www.cloud3000.com/img/chimera_sm.png" alt="Chimera logo">


# Chimirror-Server
 **Chimirror** is a technical term for **Chimera**. The *Chimera* according to Greek mythology, was a hybrid beast of Asia Minor, composed of the parts of more than one animal.

Technically the **Chimirror-Server** is a hybrid technology, composed of the parts of more than one multi-generational computing paradigm:
  
  - The Main-Frame
  - The Unix Philosophy
  - The IoT (Internet of Things)

From a simple perspective it's just a multiplexer for Advanced Web Processing. However, it's much more, it's a philosophy based on mathematical theories of concurrency known as process algebras, or process calculi, based on message passing via channels. Using one bidirectional web socket, and three application process channels (stdout, stdin, and stderr).

~~~
                 | Chimirror |
                 |  Server   |
___________________________________________________
    Client       |            |   Applications Process  
----------------------------------------------------
                 | Chimirror o|>>----->> stdin
WebSocket <<- ->>|o  I/O     o|<<-----<< stderr
                 |   Mgr.    o|<<-----<< stdout
                 |            |
----------------------------------------------------
___________________________________________________
    Client       |            |   Applications Process  
----------------------------------------------------
                 | Chimirror o|>>----->> stdin
WebSocket <<- ->>|o  I/O     o|<<-----<< stderr
                 |   Mgr.    o|<<-----<< stdout
                 |            |
----------------------------------------------------
___________________________________________________
    Client       |            |   Applications Process  
----------------------------------------------------
                 | Chimirror o|>>----->> stdin
WebSocket <<- ->>|o  I/O     o|<<-----<< stderr
                 |   Mgr.    o|<<-----<< stdout
                 |            |
----------------------------------------------------
~~~
  
---
### **How does it work?**

#### One **Chimirror-Server** supports multiple web-clients and multiple applications processes, and it manages the I/O between them. 

> There are four rules governing the Client / Application process relationship. Allowing the client and the application process to makeup their own rules.

  1. A new **application process** is created for each new **client connection**.
  2. I/O between the **client** and the **application process** is managed by the **Chimirror-Server** using standard I/O (*stdin, stdout, and stderr*) and the client WebSocket.
  3. Each client and application process relationship is defined as a **session**. If either the client or the application process exit, then the session is terminated by the Chimirror-Server.
  4. **NO BINARY DATA.** Data transmitted between the client and the application process must be in **JSON. stringify()** format. Data coming from the application process (stdout or stderr) will be formatted into a JSON string by the **Chimirror-Server**.   

Her is an example of data coming from the application process:
   ~~~javascript
    {"type" : "stdout", "payload": "base64 encoded data"}
   ~~~

  >The number of sessions supported by the **Chimirror-Server** is configurable, but limited to the hardware resources available. A small laptop should be able to support hundreds of sessions, whereas a large server might support hundreds of thousands.

>Communication designed to be whatever is agreed upon by the web client and the application process. For example you can agree to use **JSON** data structures where both client and server contain the algorithms to support the data structures and their purpose.

>It should be noted that each web client can be connected to only one applications process. The application process itself is not limited in anyway, it can create and manage it's own process tree. 

---
### Where can it be used?
>It's important to note that the application process can be written in any programming language that supports standard I/O. This is difference than most application frameworks, like nodejs, express, and socket.io all require your application to be written in JavaScript, and likewise Flask requires Python, and so on. With **Chimirror-Server** you can use any language, at any scale. 
- Single User applications (using any programming language) for desktops, laptops, or tablets.
- Small business in-house servers.
- Large Cloud Server environments.
- The server is currently built for a unix environment.
- Web Clients run on any platform with a browser that supports web sockets.
- Cli Clients on any platform with a socket library.
  
---

