/* swiplserver SWI Prolog integration library
    Author:        Sylvain Lapeyrade
    E-mail:        sylvain.lapeyrade@uca.fr
    WWW:           https://www.sylvainlapeyrade.github.io
    Copyright (c)  2021, Eric Zinda
*/

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Text.Json;
using System.Threading;

namespace PrologMachineQueryInterface
{
    class PrologError
    {
        public string _exception_json;

        public PrologError(string exception_json)
        {
            using JsonDocument doc = JsonDocument.Parse(exception_json);
            JsonElement jsonResult = doc.RootElement;

            Debug.Assert(jsonResult.GetProperty("functor").GetString() == "exception" &&
                jsonResult.GetProperty("args").ToString().Length == 1);
            this._exception_json = jsonResult.GetProperty("args")[0].ToString();

            this.Prolog();
        }

        public string Json()
        {
            return this._exception_json;
        }

        public string Prolog()
        {
            // return json_to_prolog(this._exception_json);
            return this._exception_json;
        }
    }

    public class PrologConnectionFailedError : Exception
    {
        public PrologConnectionFailedError(string jsonResult) : base(jsonResult) { }
    }

    public class PrologQueryTimeoutError : Exception
    {
        public PrologQueryTimeoutError(string jsonResult) : base(jsonResult) { }
    }

    public class PrologNoQueryError : Exception
    {
        public PrologNoQueryError(string jsonResult) : base(jsonResult) { }
    }

    public class PrologQueryCancelledError : Exception
    {
        public PrologQueryCancelledError(string jsonResult) : base(jsonResult) { }
    }

    public class PrologResultNotAvailableError : Exception
    {
        public PrologResultNotAvailableError(string jsonResult) : base(jsonResult) { }
    }

    public class PrologMQI
    {
        public int? _port;
        public string? _password;
        public Process? _process;
        public float? _query_timeout;
        public int? pending_connections;
        public string? _output_file;
        public string? _unix_domain_socket;
        public string? _mqi_traces;
        public bool _launch_mqi;
        public string? _prolog_path;
        public string? _prolog_path_args;
        public bool? connection_failed;

        public PrologMQI(
            bool launch_mqi = true,
            int? port = null,
            string? password = null,
            string? unix_domain_socket = null,
            float? query_timeout_seconds = null,
            int? pending_connection_count = null,
            string? output_file_name = null,
            string? mqi_traces = null,
            string? prolog_path = null,
            string? prolog_path_args = null)
        {
            this._port = port;
            this._password = password;
            this._process = null;
            this._query_timeout = query_timeout_seconds;
            this.pending_connections = pending_connection_count;
            this._output_file = output_file_name;
            this._unix_domain_socket = unix_domain_socket;
            this._mqi_traces = mqi_traces;
            this._launch_mqi = launch_mqi;
            this._prolog_path = prolog_path;
            this._prolog_path_args = prolog_path_args;

            this.connection_failed = false;

            OperatingSystem os = Environment.OSVersion;
            PlatformID pid = os.Platform;

            // Ensure arguments are valid
            if (this._unix_domain_socket != null)
            {
                if (pid == PlatformID.Win32NT || pid == PlatformID.Win32S || pid == PlatformID.Win32Windows || pid == PlatformID.WinCE)
                    throw new ArgumentException("Unix domain sockets are not supported on Windows");
                else if (this._port != null)
                    throw new ArgumentException("Must only provide one of: port or unix_domain_socket");
            }

            if (this._launch_mqi is false && this._output_file != null)
                throw new ArgumentException("output_file only works when launch_mqi is True");

            this.Start();
        }


        public void Start()
        {
            if (this._launch_mqi)
            {
                // File.WriteAllText("output.txt", string.Empty); // Clear output file
                // using StreamWriter file = new("output.txt", append: true);

                string swiplPath = "swipl";

                if (this._prolog_path != null)
                    swiplPath = Path.Join(this._prolog_path, "swipl");

                string launchArgs = "";

                if (this._prolog_path_args != null)
                    launchArgs += this._prolog_path_args;

                launchArgs +=
                    (
                        "--quiet"
                        + " -g"
                        + " mqi_start"
                        + " -t"
                        + " halt"
                        + " --"
                        + " --write_connection_values=true"
                    );

                if (this.pending_connections != null)
                    launchArgs += " --pending_connections=" + this.pending_connections;
                if (this._query_timeout != null)
                    launchArgs += " --query_timeout=" + this._query_timeout;
                if (this._password != null)
                    launchArgs += " --password=" + this._password;
                if (this._output_file != null)
                {
                    string finalPath = PrologFunctions.CreatePosixPath(this._output_file);
                    launchArgs += " --write_output_to_file =" + this._output_file;
                    Console.WriteLine("Writing all Prolog output to file: " + finalPath);
                }
                if (this._port != null)
                    launchArgs += " --port=" + this._port;

                if (this._unix_domain_socket != null)
                {
                    if (this._unix_domain_socket.Length > 0)
                        launchArgs += " --unix_domain_socket=" + this._unix_domain_socket;
                    else
                        launchArgs += " --unix_domain_socket";
                }

                // file.WriteLine("Prolog MQI launching swipl with args: " + launchArgs);

                try
                {
                    this._process = new Process();
                    this._process.StartInfo.FileName = swiplPath;
                    this._process.StartInfo.Arguments = launchArgs;
                    this._process.StartInfo.UseShellExecute = false;
                    this._process.StartInfo.RedirectStandardOutput = true;
                    this._process.Start();
                }
                catch (FileNotFoundException)
                {
                    throw new PrologLaunchError("The SWI Prolog executable 'swipl' could not be found on the system path, please add it.");
                }


                if (this._unix_domain_socket is null)
                {
                    string? portString = this._process.StandardOutput.ReadLine();

                    if (portString == "")
                        throw new PrologLaunchError("no port found in stdout");
                    else if (portString != null)
                    {
                        string serverPortString = portString.Trim('\n');
                        this._port = Int32.Parse(serverPortString);
                        // file.WriteLine("Prolog MQI port: " + this._port);
                    }
                }
                else
                {
                    string? domain_socket = this._process.StandardOutput.ReadLine();

                    if (domain_socket == "")
                        throw new PrologLaunchError("no Unix Domain Socket found in stdout");
                    else if (domain_socket != null)
                        this._unix_domain_socket = domain_socket.Trim('\n');
                }

                string? passwordString = this._process.StandardOutput.ReadLine();
                if (passwordString == "")
                    throw new PrologLaunchError("no password found in stdout");
                else if (passwordString != null)
                {
                    this._password = passwordString.Trim('\n');
                    // file.WriteLine("Prolog MQI password: " + this._password);
                }
            }

        }


        public PrologThread CreateThread()
        {
            return new PrologThread(this);
        }

        public int? ProcessID()
        {
            if (this._process != null)
                return this._process.Id;
            else
                return null;
        }
    }

    public class PrologLaunchError : Exception
    {
        public PrologLaunchError(string message) : base(message) { }
    }


    public class PrologThread
    {
        public PrologMQI _prolog_server;
        public Socket? _socket;
        public string? communication_thread_id;
        public string? goal_thread_id;
        public int _heartbeat_count;
        public int? _server_protocol_major;
        public int? _server_protocol_minor;

        public PrologThread(PrologMQI prologMQI)
        {
            this._prolog_server = prologMQI;
            this._socket = null;
            this.communication_thread_id = null;
            this.goal_thread_id = null;
            this._heartbeat_count = 0;
            this._server_protocol_major = null;
            this._server_protocol_minor = null;

            this.Start();
        }

        public void Start()
        {
            // using StreamWriter file = new("output.txt", append: true);

            // file.WriteLine("Entering PrologThread: " + this._prolog_server.process_id());

            if (this._socket != null)
                return;

            if (this._prolog_server.ProcessID() is null)
                this._prolog_server.Start();

            Dictionary<string, int?> prologAddress = new();

            if (this._prolog_server._unix_domain_socket != null)
            {
                prologAddress.Add(this._prolog_server._unix_domain_socket, this._prolog_server._port);
                this._socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            }
            else
            {
                prologAddress.Add("127.0.0.1", this._prolog_server._port);
                this._socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            }

            // file.WriteLine("PrologMQI connecting to Prolog at: " + prologAddress.First().Key + ":" + prologAddress.First().Value);

            int connect_count = 0;
            while (connect_count < 3)
            {
                try
                {
                    this._socket.Connect(IPAddress.Parse(prologAddress.First().Key), prologAddress.First().Value ?? default);
                    break;
                }
                catch (SocketException)
                {
                    // file.WriteLine("Server not responding %s", prologAddress);
                    connect_count += 1;
                    Thread.Sleep(1);
                    continue;
                }
            }
            if (connect_count == 3)
                throw new SocketException((int)SocketError.ConnectionRefused);

            this.Send(this._prolog_server._password ?? "Null Password");

            string result = this.Receive();

            using JsonDocument doc = JsonDocument.Parse(result);
            JsonElement jsonResult = doc.RootElement;


            // file.WriteLine("\nPrologMQI received: " + jsonResult.GetProperty("functor"));

            if (jsonResult.GetProperty("functor").ToString() != "true")
                throw new PrologLaunchError($"Failed to accept password: {jsonResult}");
            else
            {
                this.communication_thread_id = jsonResult.GetProperty("args")[0][0][0].GetProperty("args")[0].ToString();
                this.goal_thread_id = jsonResult.GetProperty("args")[0][0][0].GetProperty("args")[1].ToString();

                // file.WriteLine("PrologMQI server protocol: " + this.communication_thread_id + " " + this.goal_thread_id);

                if (jsonResult.GetProperty("args")[0][0].GetArrayLength() > 1)
                {
                    JsonElement versionTerm = jsonResult.GetProperty("args")[0][0][1];
                    this._server_protocol_major = versionTerm.GetProperty("args")[0].GetInt32();
                    this._server_protocol_minor = versionTerm.GetProperty("args")[1].GetInt32();
                }
                else
                {
                    this._server_protocol_major = 0;
                    this._server_protocol_minor = 0;
                }

                // file.WriteLine("PrologMQI server protocol: " + this._server_protocol_major + " " + this._server_protocol_minor);

                this.CheckProtocolVersion();
            }
        }

        public void CheckProtocolVersion()
        {
            int required_server_major = 1;
            int required_server_minor = 0;

            if (this._server_protocol_major == 0 && this._server_protocol_minor == 0)
                return;

            if (this._server_protocol_major == required_server_major && this._server_protocol_minor == required_server_minor)
                return;

            throw new PrologLaunchError($"This version of swiplserver requires MQI major version {required_server_major} and minor version >= {required_server_minor}. The server is running MQI '{this._server_protocol_major}.{this._server_protocol_minor}");
        }

        public string Receive()
        {
            // using StreamWriter file = new("output.txt", append: true);

            byte[] buffer = new byte[4096];

            if (this._socket is null)
                throw new NullReferenceException("Socket is null");

            int iRx = this._socket.Receive(buffer);

            string msg = Encoding.ASCII.GetString(buffer, 0, iRx);

            // file.WriteLine("\nReceived: " + msg);

            msg = msg[(msg.IndexOf('.') + 1)..];

            return msg;
        }

        public void Send(string value)
        {
            // using StreamWriter file = new("output.txt", append: true);

            value = value.Trim();
            value = value.Trim('\n');
            value = value.Trim('.');
            value += ".\n";

            // file.WriteLine("PrologMQI send: " + value);

            byte[] valueBytes = Encoding.UTF8.GetBytes(value);
            string utf8value = Encoding.UTF8.GetString(valueBytes);

            // file.WriteLine("Utf8 Value: " + utf8value);

            int messageLen;
            if (this._server_protocol_major == 0)
                messageLen = value.Length;
            else
                messageLen = utf8value.Length;

            string msgHeader = messageLen.ToString() + ".\n";
            byte[] messageLenBytes = Encoding.UTF8.GetBytes(msgHeader);

            // file.Write("Message Len: ");
            // for (int i = 0; i < messageLenBytes.Length; i++)
            // file.Write(messageLenBytes[i]);


            // file.Write("\nMessage Content: ");
            // for (int i = 0; i < valueBytes.Length; i++)
            // file.Write(valueBytes[i]);

            if (this._socket != null)
            {
                this._socket.Send(messageLenBytes);
                this._socket.Send(valueBytes);
            }
        }

        public void Stop()
        {
            if (this._socket != null)
            {
                if (this._prolog_server.connection_failed ?? false)
                {
                    this.Send("close.\n");
                    this.ReturnPrologResponse();
                }
            }
        }


        public List<Tuple<string, string>> Query(string value, float? query_timeout_seconds = null)
        {
            if (this._socket is null)
                this.Start();

            value = value.Trim();
            value = value.Trim('\n');

            string? timeoutString = query_timeout_seconds.ToString();
            if (query_timeout_seconds is null)
                timeoutString = "_";

            this.Send($"run(({value}), {timeoutString}).\n");

            return this.ReturnPrologResponse();
        }

        public void QueryAsync(string value, bool find_all, float? query_timeout_seconds = null)
        {
            if (this._socket is null)
                this.Start();

            value = value.Trim();
            value = value.Trim('\n');

            string? timeoutString = query_timeout_seconds.ToString();
            if (query_timeout_seconds is null)
                timeoutString = "_";

            this.Send($"run_async(({value}), {timeoutString}, {find_all.ToString().ToLower()}).\n");

            this.ReturnPrologResponse();
        }

        public void CancelQueryAsync()
        {
            this.Send("cancel_async.\n");
            this.ReturnPrologResponse();
        }

        public List<Tuple<string, string>> QueryAsyncResult(float? wait_timeout_seconds = null)
        {
            // StreamWriter file = new("output.txt", append: true);

            string? timeoutString = wait_timeout_seconds.ToString();
            if (wait_timeout_seconds is null)
                timeoutString = "-1";

            // file.WriteLine("\nquery_async_result: " + timeoutString);

            this.Send($"async_result({timeoutString}).\n");

            return this.ReturnPrologResponse();
        }

        public void HaltServer()
        {
            this.Send("quit.\n");
            this.ReturnPrologResponse();
            this._prolog_server.connection_failed = true;
        }

        public List<Tuple<string, string>> ReturnPrologResponse()
        {
            // using StreamWriter file = new("output.txt", append: true);
            string result = this.Receive();

            // file.WriteLine("\nReceive: " + result);

            List<Tuple<string, string>> answerList = new();


            using JsonDocument doc = JsonDocument.Parse(result);
            JsonElement jsonResult = doc.RootElement;

            // file.WriteLine("Prolog Response:" + jsonResult);

            if (jsonResult.ToString() != "false" && jsonResult.GetProperty("functor").ToString() == "exception")
            {
                if (jsonResult.GetProperty("args")[0].ToString() == "no_more_results")
                    answerList.Add(new Tuple<string, string>("null", "null"));
                else if (jsonResult.GetProperty("args")[0].ToString() == "connection_failed")
                    this._prolog_server.connection_failed = true;
                else if (!typeof(string).IsInstanceOfType(jsonResult.GetProperty("args")[0]))
                    throw new PrologLaunchError(jsonResult.ToString());

                switch (jsonResult.GetProperty("args")[0].ToString())
                {
                    case "connection_failed":
                        throw new PrologConnectionFailedError(jsonResult.GetProperty("args")[0].ToString());
                    case "time_limit_exceeded":
                        throw new PrologQueryTimeoutError(jsonResult.GetProperty("args")[0].ToString());
                    case "no_query":
                        throw new PrologNoQueryError(jsonResult.GetProperty("args")[0].ToString());
                    case "cancel_goal":
                        throw new PrologQueryCancelledError(jsonResult.GetProperty("args")[0].ToString());
                    case "result_not_available":
                        throw new PrologResultNotAvailableError(jsonResult.GetProperty("args")[0].ToString());
                }
            }
            else
            {
                if (jsonResult.ToString() == "false" || jsonResult.GetProperty("functor").ToString() == "false")
                    return new List<Tuple<string, string>>() { new Tuple<string, string>("false", "null") };
                else if (jsonResult.ToString() == "true" || jsonResult.GetProperty("functor").ToString() == "true")
                {
                    for (int i = 0; i < jsonResult.GetProperty("args")[0].GetArrayLength(); i++)
                    {
                        if (jsonResult.GetProperty("args")[0][i].GetArrayLength() == 0)
                            answerList.Add(new Tuple<string, string>("true", "null"));
                        else
                        {
                            for (int j = 0; j < jsonResult.GetProperty("args")[0][i].GetArrayLength(); j++)
                            {
                                answerList.Add(new Tuple<string, string>(jsonResult.GetProperty("args")[0][i][j].GetProperty("args")[0].ToString(), jsonResult.GetProperty("args")[0][i][j].GetProperty("args")[1].ToString()));
                            }
                        }
                    }

                    if (answerList.Count > 0 && answerList.ElementAt(0).Item1 == "true")
                        answerList.Add(new Tuple<string, string>("true", "null"));
                    else
                        return answerList;
                }
            }
            return answerList;
        }
    }


    public static class PrologFunctions
    {
        public static string CreatePosixPath(string os_path)
        {
            return os_path.Replace("\\", "/");
        }

        public static bool IsPrologFunctor(JsonElement json_term)
        {
            return json_term.TryGetProperty("functor", out _) && json_term.TryGetProperty("args", out _);
        }

        public static bool IsPrologVariable(JsonElement json_term)
        {
            return json_term.GetProperty("args")[0].ToString() == "test" || json_term[0].GetProperty("args")[0].ToString() == "_";
        }

        public static bool IsPrologAtom(JsonElement json_term)
        {
            return !IsPrologVariable(json_term);
        }

        public static JsonElement PrologName(JsonElement json_term)
        {
            if (IsPrologAtom(json_term) || IsPrologVariable(json_term))
                return json_term;
            else
                return json_term.GetProperty("functor");
        }

        public static JsonElement PrologArgs(JsonElement json_term)
        {
            return json_term.GetProperty("args");
        }

        // void quote_prolog_identifier(identifier: str):
        //     """
        //     Surround a Prolog identifier with '' if Prolog rules require it.
        //     """
        //     if not is_prolog_atom(identifier):
        //         return identifier
        //     else:
        //         mustQuote = is_prolog_atom(identifier) and(
        //             len(identifier) == 0
        //             or not identifier[0].isalpha()
        //             or
        //             # characters like _ are allowed without quoting
        //             not identifier.translate({ ord(c): "" for c in "_"}).isalnum()
        //         )

        //         if mustQuote:
        //             return f"'{identifier}'"
        //         else:
        //             return identifier


        // def json_to_prolog(json_term):
        //     """
        //     Convert json_term from the Prolog JSON format to a string that represents the term in the Prolog language. See `PrologThread.query` for documentation on the Prolog JSON format.
        //     """
        //     if is_prolog_functor(json_term):
        //         argsString = [json_to_prolog(item) for item in prolog_args(json_term)]
        //         return f"{quote_prolog_identifier(prolog_name(json_term))}({', '.join(argsString)})"
        //     elif is_prolog_list(json_term):
        //         listString = [json_to_prolog(item) for item in json_term]
        //         return f"[{', '.join(listString)}]"
        //     else:
        //         # must be an atom, number or variable
        //         return str(quote_prolog_identifier(json_term))
    }


    public class Test
    {
        public static void Main()
        {
            PrologMQI mqi = new();
            PrologThread prologThread = mqi.CreateThread();

            /*************************************************************************
            TEST 0 : Test of the consult command to use a prolog file 
            *************************************************************************/
            Console.WriteLine("TEST 0 : Consult command and query afeter consult\n");
            Console.WriteLine("Query: consult(test).");
            Console.WriteLine(prologThread.Query("consult(test)").ElementAt(0).Item1);
            Console.WriteLine("Query: father(bob).");
            Console.WriteLine(prologThread.Query("father(bob)").ElementAt(0).Item1);

            /*************************************************************************
            TEST 1 : Query with only one answer (i.e. true ou false)
            *************************************************************************/
            Console.WriteLine("\n\nTEST 1 : Query with only one answer (i.e. true ou false)\n");
            Console.WriteLine("Query: assertz(father(michael)).");
            Console.WriteLine(prologThread.Query("assertz(father(michael))").ElementAt(0).Item1);
            Console.WriteLine("\nQuery: father(michael).");
            Console.WriteLine(prologThread.Query("father(michael)").ElementAt(0).Item1);
            Console.WriteLine("\nQuery: father(paul).");
            Console.WriteLine(prologThread.Query("father(paul)").ElementAt(0).Item1);

            /*************************************************************************
            TEST 2 : Query with one atom and multiple answers
            *************************************************************************/
            Console.WriteLine("\n\nTEST 2 : Query with one argument and multiple answers\n");
            Console.WriteLine("Query: father(X)\n");
            prologThread.QueryAsync("father(X)", false);

            bool test2_more_results = true;
            while (test2_more_results)
            {
                List<Tuple<string, string>> test2_result = prologThread.QueryAsyncResult();
                for (int i = 0; i < test2_result.Count; i++)
                {
                    if (test2_result.ElementAt(i).Item1 == "null" && test2_result.ElementAt(i).Item2 == "null")
                    {
                        Console.WriteLine("No more results");
                        test2_more_results = false;
                    }
                    else
                        Console.WriteLine(test2_result.ElementAt(i).Item1 + " = " + test2_result.ElementAt(i).Item2);
                }
            }

            /*************************************************************************
            TEST 3 : Query with two arguments and multiple answers
            *************************************************************************/
            Console.WriteLine("\n\nTEST 3 : Query with two arguments and multiple answers\n");
            Console.WriteLine("Query: mother(X, Y)\n");
            prologThread.QueryAsync("mother(X, Y)", false);

            bool test3_more_results = true;
            while (test3_more_results)
            {
                List<Tuple<string, string>> test3_results = prologThread.QueryAsyncResult();
                for (int i = 0; i < test3_results.Count; i++)
                {
                    if (test3_results.ElementAt(i).Item1 == "null" && test3_results.ElementAt(i).Item2 == "null")
                    {
                        Console.WriteLine("No more results");
                        test3_more_results = false;
                    }
                    else
                        Console.WriteLine(test3_results.ElementAt(i).Item1 + " = " + test3_results.ElementAt(i).Item2);
                }
            }

            /*************************************************************************
            TEST 4 : Queries with threads
            *************************************************************************/
            Console.WriteLine("\n\nTEST 4: Queries with threads\n");
            Console.WriteLine("Query: prolog_thread2.query_async(sleep(1), father(michael))");
            Console.WriteLine("Query: prolog_thread1.query_async(father(kevin))");
            PrologThread prologThread1 = mqi.CreateThread();
            PrologThread prologThread2 = mqi.CreateThread();
            prologThread1.QueryAsync("sleep(1), father(michael)", false);
            prologThread2.QueryAsync("father(kevin)", false);

            Console.WriteLine("Thread 2: " + prologThread2.QueryAsyncResult().ElementAt(0).Item1);
            Console.WriteLine("Thread 1: " + prologThread1.QueryAsyncResult().ElementAt(0).Item1);

            /*************************************************************************
            TEST 5 : Query response time
            *************************************************************************/
            Console.WriteLine("\n\nTEST 5: Query response time\n");
            Console.WriteLine("Query: time(father(bob)).");
            Stopwatch timer = new();
            timer.Start();
            Console.WriteLine(prologThread.Query("time(father(bob))").ElementAt(0).Item1);
            Console.WriteLine("Time elapsed: {0}", timer.Elapsed);
            timer.Restart();
            Console.WriteLine(prologThread.Query("time(father(bob))").ElementAt(0).Item1);
            Console.WriteLine("Time elapsed: {0}", timer.Elapsed);


            /*************************************************************************
            TEST 6 : Answers with arrays
            *************************************************************************/
            Console.WriteLine("\n\nTEST 6: Answers with arrays\n");
            Console.WriteLine("Query: uncle([Col, Row], X, Y).");
            prologThread.QueryAsync("uncle([Col, Row], X, Y)", false);

            bool test6_more_results = true;
            while (test6_more_results)
            {
                List<Tuple<string, string>> test6_results = prologThread.QueryAsyncResult();
                for (int i = 0; i < test6_results.Count; i++)
                {
                    if (test6_results.ElementAt(i).Item1 == "null" && test6_results.ElementAt(i).Item2 == "null")
                    {
                        Console.WriteLine("No more results");
                        test6_more_results = false;
                    }
                    else
                        Console.WriteLine(test6_results.ElementAt(i).Item1 + " = " + test6_results.ElementAt(i).Item2);
                }
            }

        }
    }
}