using System.Data.SqlClient;
using System.Configuration;

// 
// Import log4net classes
// Fine control over the logging
//
using log4net;

namespace EloquenceMigrationClient
{
    /// <summary>
    /// SQL class provides the generic methods to connect to a SQL Server and execute SQL statements
    /// <list type="bullet">
    /// <item>
    /// <term>SQL</term>
    /// <description>Class Constructor initializes the connection to the default server in the app.config</description>
    /// </item>
    /// <item>
    /// <term>Execute</term>
    /// <description>Executes an non-query SQL statement possibly with comments but where all "GO" strings MUST BE replaced with ";".
    /// Multiple SQL statements separated by ";" are allowed but only the results of the latest SQL statement is returned.
    /// </description>
    /// </item>
    /// </list>
    /// </summary>
    public class SQL
    {
        //
        // Setup logging for this class. See log.txt for log messages and app.config for logging configuration.
        //
        private static readonly ILog log = LogManager.GetLogger(typeof(SQL));
        private SqlConnection DB = new SqlConnection(ConfigurationManager.ConnectionStrings["default"].ConnectionString);
        private SqlCommand DBCmd = new SqlCommand();
        //
        // DBData will be use for SQL queries in the future.
        // We did NOT need to query in the first release.
        //
        private SqlDataReader DBData;

        /// <summary>
        /// SQL.SQL initializes the connection to the default server in the app.config.
        /// Runs automatically each time a new object of this class is created.
        /// </summary>
        public SQL()
        {
            DB.Open();
        }

        /// <summary>
        /// SQL.Execute runs a non-query SQL statement and returns the number of rows changed.
        /// SQL must be a complete and valid SQL statement or multiple statements separated by ";".
        /// SQL CANNOT have "GO" in it if it does then a syntax error exception will occur.
        /// </summary>
        public int Execute(string sql)
        {

            int result = 0;

            //
            // sql must be a valid SQL statement!
            //
            if (sql.Length > 0)
            {
                DBCmd.Connection = DB;
                DBCmd.CommandText = sql;
                log.DebugFormat("SQL={0}", DBCmd.CommandText);
                result = DBCmd.ExecuteNonQuery();
            }

            return result;
        }
    }
}