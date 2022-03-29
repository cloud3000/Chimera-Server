using System;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Diagnostics;
using System.Threading;

// 
// Import log4net classes
// Fine control over the logging
//
using log4net;

namespace HC
{
    class NW
    {
        //
        // Setup logging for this class. See log.txt for log messages and app.config for logging configuration.
        //
        private static readonly ILog log = LogManager.GetLogger(typeof(NW));

        // Data buffer
        byte[] bytes = new byte[4096];

        // Create a TCP/IP  socket.  
        Socket sock;
        string Host;
        ushort Servport;
        string Serv;
        string Passwd;
        string Security;
        string Username = Environment.UserName;
        string Domain = Environment.UserDomainName;
        ushort Device = 900;
        int PID = Process.GetCurrentProcess().Id;
        int MaxWait = 30 * 1000; // milliseconds

        private bool WaitForAvailable(int numofBytes)
        {
            int maxwait = MaxWait;
            bool result = false;

            while (sock.Available < numofBytes)
            { 
                if (maxwait < 0)
                {
                    break;
                }
                Thread.Sleep(1);
                maxwait--;
            }

            if (sock.Available < numofBytes)
            {
                log.DebugFormat("WaitForAvailable timed out waiting for {0} bytes after {1} milliseconds", numofBytes, MaxWait);
                result = false;
            }
            else
            {
                result = true;
            }

            return result;
        }

        private short readack()
        {
            short result = 0;
            byte[] ack = { 0x3f }; // '?'
            byte Y = 0x59; // 'Y'
            byte y = 0x79; // 'y'
            string Ack;

            try
            {
                sock.Receive(ack);
                Ack = Encoding.ASCII.GetString(ack);

                if ((ack[0] == Y) || (ack[0] == y))
                {
                    // Ack was received
                    result = 0;
                }
                else
                {
                    // Ack was not received
                    result = 10;
                }
            }
            catch (ArgumentNullException ane)
            {
                log.ErrorFormat("ArgumentNullException : {0}", ane.ToString());
                result = 10;
            }
            catch (SocketException se)
            {
                log.ErrorFormat("SocketException : {0}", se.ToString());
                result = 10;
            }
            catch (Exception e)
            {
                log.ErrorFormat("Unexpected exception : {0}", e.ToString());
                result = 10;
            }

            return result;

        }

        private short writeack()
        {
            short result = 0;
            byte[] ack = { 0x59 }; // 'Y'

            try
            {
                sock.Send(ack);
            }
            catch (ArgumentNullException ane)
            {
                log.ErrorFormat("ArgumentNullException : {0}", ane.ToString());
                result = 10;
            }
            catch (SocketException se)
            {
                log.ErrorFormat("SocketException : {0}", se.ToString());
                result = 10;
            }
            catch (Exception e)
            {
                log.ErrorFormat("Unexpected exception : {0}", e.ToString());
                result = 10;
            }

            return result;

        }
        public short nwconnect(ushort servport, string serv, string passwd, string host, out short err)
        {
            short result = 0;

            err = 0;
            Host = host;
            Serv = serv;
            Servport = servport;
            Passwd = passwd;

            if (Host.Length > 8)
            {
                Host = Host.Substring(0, 8);
            }

            if (Serv.Length > 8)
            {
                Serv = Serv.Substring(0, 8);
            }

            if (Passwd.Length > 8)
            {
                Passwd = Passwd.Substring(0, 8);
            }

            if (Username.Length > 16)
            {
                Username = Username.Substring(0, 16);
            }

            // Establish the remote endpoint for the socket.  
            // This example uses port 11000 on the local computer.  
            IPHostEntry ipHostInfo = Dns.GetHostEntry(host);
            IPAddress ipAddress = ipHostInfo.AddressList[0];
            IPEndPoint remoteEP = new IPEndPoint(ipAddress, Servport);

            try
            {
                // Create a TCP/IP  socket.  
                sock = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

                // Send operations will time-out if confirmation 
                // is not received within 1000 milliseconds.
                sock.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.SendTimeout, 1000);

                // The socket will linger for 10 seconds after Socket.Close is called.
                LingerOption lingerOption = new LingerOption(true, 10);

                sock.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.Linger, lingerOption);

                // Turn Keep alives on
                sock.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.KeepAlive, true);

                sock.Connect(remoteEP);

                //sprintf(sec_rec, "SECURITY%-16s%-8s%-8s%04d%08d%-8s", username, passwd, service, device, getpid(), host);
                Security = String.Format("SECURITY{0,-16}{1,-8}{2,-8}{3,4:D4}{4,8:D8}{5,-8}", Username, Passwd, Serv, Device, PID, Host);

                // Send Security record
                //  
                int bytesSent = sock.Send(Encoding.ASCII.GetBytes(Security));

                result = readack();

                if (result == 0)
                {
                    byte[] securityReply = new byte[14];
                    string secreply;

                    // get Security reply
                    sock.Receive(securityReply);

                    secreply = Encoding.ASCII.GetString(securityReply);
                    if (secreply.Length != 14)
                    {
                        //
                        // Security reply too short
                        //
                        result = 9;
                    }
                    else if (secreply.Substring(0,6) != "PASSED")
                    {
                        //
                        // Security failed
                        //
                        result = 9;
                    }
                    else
                    {
                        //
                        // Success
                        //
                        result = 0;
                        writeack();
                    }
                }
            }
            catch (ArgumentNullException ane)
            {
                log.ErrorFormat("ArgumentNullException : {0}", ane.ToString());
                result = 9;
            }
            catch (SocketException se)
            {
                log.ErrorFormat("SocketException : {0}", se.ToString());
                result = 9;
            }
            catch (Exception e)
            {
                log.ErrorFormat("Unexpected exception : {0}", e.ToString());
                result = 9;
            }

            return result;
        }

        //nwsend (int *desc, short *datalen, char *buf, short *err)
        public short nwsend(short datalen, string buf, out short err)
        {
            short result = 0;
            err = 0;
            short buflen = (short) buf.Length;

            short nbuflen = IPAddress.HostToNetworkOrder(buflen);

            int bytesSent = sock.Send(BitConverter.GetBytes(nbuflen));
            bytesSent = sock.Send(Encoding.ASCII.GetBytes(buf));
            result = readack();

            return result;
        }

        //short nwreceive_tcp (int *desc, short *datalen, char *buf, short *err)
        public short nwreceive(ref short datalen, out string buf, out short err)
        {
            short result = 0;
            byte[] recvlen = new byte[2];
            short nRecvLen = 0;
            short RecvLen = 0;
            err = 0;
            buf = "";

            //
            // First set the socket to be non-blocking
            //
            if (! sock.Blocking)
            {
                sock.Blocking = false;
            }
            datalen = 0;

            //
            // Now wait for 2 bytes len to be received
            //
            log.DebugFormat("nwreceive expecting to get 2 bytes of receive len");
            if (WaitForAvailable(2))
            {
                //
                // Read 2 byte len in network order and convert to host order
                sock.Receive(recvlen);
                nRecvLen = BitConverter.ToInt16(recvlen, 0);
                RecvLen = IPAddress.NetworkToHostOrder(nRecvLen);
                log.DebugFormat("nwreceive expecting to get {0} bytes", RecvLen);
                if (WaitForAvailable(RecvLen))
                {
                    sock.Receive(bytes, RecvLen, SocketFlags.None);
                    result = RecvLen;
                    writeack();
                    buf = Encoding.ASCII.GetString(bytes, 0, RecvLen);
                    datalen = RecvLen;
                }
                else
                {
                    err = 10;
                }

            }
            else
            {
                err = 10;
            }

            return result;
        }
    }
}
