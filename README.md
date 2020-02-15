
# AWP-Server
### What is it?
<div style="display:flex; border-width:20px;border-color:red;">
  <div style="width:%;background-color:black; border-radius : 49%;">
    <img  src="http://www.cloud3000.com/img/chimera.png" alt="Chimera logo" style="width : 100%;">
  </div>
  <div>
    <p style="color:grey;font-size:16px;padding:10px">
      The Chimera according to Greek mythology, was a hybrid creature of Lycia in Asia Minor, composed of the parts of more than one animal.
    </P>
  </div>
</div>


  
- The concept for the AWP-Server is based on three multi-generational computing paradigm's:
  - The Main-Frame
  - The Unix Philosophy
  - The IoT (Internet of Things)
- It's basically a multiplexer for Advanced Web Processing.
  
To make use of, or to criticize, you must know what a Main-Frame is, you must understand the Unix Philosophy, and the Internet of Things. Knowing the history is critical for understanding, if you're at a loss then try Duck.com

---
### How does it work?
- One AWP server supports multiple web-clients and multiple processes.
  - A new process is created for each new web client connection.
  + A web client is connected to the new process on the server using a client web socket and process's standard I/O (stdin, stdout, and stderr).
  + Communication designed to be whatever is agreed upon by the web client and the server process. For example JSON data structures where both client and server contain the algorithms to support the data structures.
- Each web client and server process relationship is defined as a **session**.
- The number of sessions supported by 1 AWP-Server is limited to the hardware resources available. A small laptop should be able to support hundreds of sessions, whereas a large server might support hundreds of thousands.

---
### Where can it be used?
- Single User applications for desktops, laptops, or tablets.
- Small business in-house servers.
- Large Cloud Server environments.
- The server is currently built for a unix environment.
- Web Clients run on any platform with a browser that supports web sockets.
- Cli Clients on any platform with a socket library.
  
---

  


In development: Supports multiple browser-based sessions, each session is linked to a back-end Process manager.
As of Jan 2017 ATP supports multiple sessions without security, and the process manager support one process for each session.
The process used to test is 'animal', which which accepts input from stdin, and write output to stdout and stderr.
All three standard I/O channels are working, with multiple session.

I/O for each session process is handled as follows:
  stdin is BLOCKED I/O, so the process will block when reading from stdin.
  stdout is non-blocking, and will send events to the process manager, then read (as data),  forwarded to the browser.
  stderr is handled the same as stdout, except the events are identified as error messages, forwarded to the browser.

NEXT STEPS;
Add support for multiple processes within each session.
First process will be login and security checks, with session creation.
Second process will be a dummy application process, with session validation capability.

After the process management is complete and working properly, then I'll move on to Forms management.
Then database and application intrinsics- [ATP-Server](#atp-server).
