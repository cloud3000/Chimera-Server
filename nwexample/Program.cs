using System;
// 
// Import log4net classes
// Fine control over the logging
//
using log4net;

namespace EloquenceMigrationClient
{
    /// <summary>
    /// NWClient just connects to a NWServer and then receives SQL from that server
    /// <list type="bullet">
    /// <item>
    /// <term>StartClient</term>
    /// <description>Starts the NWClient</description>
    /// </item>
    /// </list>
    /// </summary>
    class NWClient
    {
        private static HC.NW conn = new HC.NW();
        private static string buffer;
        private static short buflen = 4096;
        private static string sqlbuffer;
        private static short err = 0;
        private static SQL sqlengine = new SQL();
        //
        // Setup logging for this class. See log.txt for log messages and app.config for logging configuration.
        //
        private static readonly ILog log = LogManager.GetLogger(typeof(NWClient));
        private static string Server = "balpbatch2p.harlandclarke.local";
        private static string Service = "MIG01";
        private static ushort ServicePort = 30830;
        private static string Password = "NONE";

        private static void ReceiveSQL()
        {
            int Total = 0;
            int Check = 10000;
            int sqlresult = 0;

            //
            // This NWServers expects to receive 36 bytes so we send it 36 spaces.
            //
            string sendDB = String.Format("{0,36}", " ");
            short result = conn.nwsend(36, sendDB, out err);

            err = 0;
            result = 0;
            sqlbuffer = "";
            while (err == 0 && buflen > 0) {
                buflen = 4096;
                result = conn.nwreceive(ref buflen, out buffer, out err);
                log.InfoFormat("NWReceive returns: {0}" , buffer);
                //
                // Build up the SQL from the lines received from the Server,
                // and separate them with system's newline string (CR LF "\r\n" on Windows)
                //
                sqlbuffer = sqlbuffer + Environment.NewLine + buffer;
                //
                // The service we are connected to is a data migration service.
                // It sends SQL INSERTS & CREATE statements.
                // The nature of network communication is that these SQL statements may span multiple packets.
                // So here we contniue to recv packets and append to the SQL (sqlbuffer).
                // The service will signal execution by sending "GO" in one packet, then the sqlbuffer is executed.
                // REQUIRED: The GO is expected to be by itself.
                // If the Server logic is changed, then this may need TO CHANGE as well!
                //
                if (sqlbuffer.IndexOf("GO") > -1)
                {
                    sqlbuffer = sqlbuffer.Replace("GO", ";");
                    log.InfoFormat("Found GO {0}", sqlbuffer);
                    sqlresult = sqlengine.Execute(sqlbuffer);
                    sqlbuffer = "";
                }
                
                //
                // Track total bytes from the Server for progress tracking
                // 
                Total = Total + buflen;
                if (Total > Check)
                {
                    log.InfoFormat("Progess Total bytes received {0}", Total);
                    Check = Check + 10000;
                }
            }
            log.InfoFormat("Total bytes received from Server {0}", Total);
        }

        /// <summary>
        /// NWClient.StartClient connects to the NWServer and then starts receiving SQL statements
        /// </summary>
        public static void StartClient()
        {
            short error = -1;
  
            try
            {
                // Connect the NW service on remote host (.Net makes it hard to use the service name so we provide the service port too)
                short result = conn.nwconnect(ServicePort, Service, Password, Server, out error);
                if (result != 0)
                {
                    log.Error("ERROR: nwconnect failed. Aborting ...");
                }
                else
                {
                    // Connected; Do application stuff here
                    ReceiveSQL();
                }
            }
            catch (Exception e)
            {
                log.ErrorFormat("FAILURE Exception={0}", e.ToString());
            }
        }
    }

    class Program
    {
        static int Main(string[] args)
        {
            NWClient.StartClient();
            return 0;
        }
    }
}
