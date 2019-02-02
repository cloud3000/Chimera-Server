# ATP-Server
Advanced Terminal Process server.

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
Then database and application intrinsics.
