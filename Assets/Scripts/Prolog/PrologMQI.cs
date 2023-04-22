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

namespace Prolog
{
    /*class PrologError
    {
        private readonly string _exceptionJson;

        public PrologError(string exceptionJson)
        {
            using JsonDocument doc = JsonDocument.Parse(exceptionJson);
            JsonElement jsonResult = doc.RootElement;
        
            Debug.Assert(jsonResult.GetProperty("functor").GetString() == "exception" &&
                jsonResult.GetProperty("args").ToString().Length == 1);
            _exceptionJson = jsonResult.GetProperty("args")[0].ToString();
        
            Prolog();
        }

        public string Json()
        {
            return _exceptionJson;
        }

        private string Prolog()
        {
            return json_to_prolog(_exception_json);
            return _exceptionJson;
        }
    }*/

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

    public class PrologMqi
    {
        public int? Port;
        public string Password;
        private Process _process;
        private readonly float? _queryTimeout;
        private readonly int? _pendingConnections;
        private readonly string _outputFile;
        public string UnixDomainSocket;
        private readonly string _mqiTraces;
        private readonly bool _launchMqi;
        private readonly string _prologPath;
        private readonly string _prologPathArgs;
        public bool ConnectionFailed;

        public PrologMqi(bool launchMqi = true, int? port = null!, string password = null, string unixDomainSocket = null,
            float? queryTimeoutSeconds = null, int? pendingConnectionCount = null, string outputFileName = null,
            string prologPath = null, string prologPathArgs = null, string mqiTraces = null
            )
        {
            Port = port;
            Password = password;
            _process = null;
            _queryTimeout = queryTimeoutSeconds;
            _pendingConnections = pendingConnectionCount;
            _outputFile = outputFileName;
            UnixDomainSocket = unixDomainSocket;
            _mqiTraces = mqiTraces;
            _launchMqi = launchMqi;
            _prologPath = prologPath;
            _prologPathArgs = prologPathArgs;

            ConnectionFailed = false;

            var os = Environment.OSVersion;
            var pid = os.Platform;

            // Ensure arguments are valid
            if (UnixDomainSocket != null)
            {
                if (pid is PlatformID.Win32NT or PlatformID.Win32S or PlatformID.Win32Windows or PlatformID.WinCE)
                    throw new ArgumentException("Unix domain sockets are not supported on Windows");
                else if (Port != null)
                    throw new ArgumentException("Must only provide one of: port or unix_domain_socket");
            }

            if (_launchMqi is false && _outputFile != null)
                throw new ArgumentException("output_file only works when launch_mqi is True");

            Start();
        }


        public void Start()
        {
            if (!_launchMqi) return;
            // File.WriteAllText("output.txt", string.Empty); // Clear output file
            // using StreamWriter file = new("output.txt", append: true);

            var swiplPath = "swipl";
            // var swiplPath = "/opt/homebrew/bin/swipl";

            if (_prologPath != null)
                swiplPath = Path.Join(_prologPath, "swipl");

            var launchArgs = "";

            if (_prologPathArgs != null)
                launchArgs += _prologPathArgs;

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

            if (_pendingConnections != null)
                launchArgs += " --pending_connections=" + _pendingConnections;
            if (_queryTimeout != null)
                launchArgs += " --query_timeout=" + _queryTimeout;
            if (Password != null)
                launchArgs += " --password=" + Password;
            if (_outputFile != null)
            {
                var finalPath = PrologFunctions.CreatePosixPath(_outputFile);
                launchArgs += " --write_output_to_file =" + _outputFile;
                Console.WriteLine("Writing all Prolog output to file: " + finalPath);
            }
            if (Port != null)
                launchArgs += " --port=" + Port;

            if (UnixDomainSocket != null)
            {
                if (UnixDomainSocket.Length > 0)
                    launchArgs += " --unix_domain_socket=" + UnixDomainSocket;
                else
                    launchArgs += " --unix_domain_socket";
            }

            // file.WriteLine("Prolog MQI launching swipl with args: " + launchArgs);

            try
            {
                _process = new Process();
                _process.StartInfo.FileName = swiplPath;
                _process.StartInfo.Arguments = launchArgs;
                _process.StartInfo.UseShellExecute = false;
                _process.StartInfo.RedirectStandardOutput = true;
                _process.Start();
            }
            catch (FileNotFoundException)
            {
                throw new PrologLaunchError("The SWI Prolog executable 'swipl'" +
                                            " could not be found on the system path, please add it.");
            }


            if (UnixDomainSocket is null)
            {
                var portString = _process.StandardOutput.ReadLine();

                if (portString == "")
                    throw new PrologLaunchError("no port found in stdout");
                if (portString != null)
                {
                    var serverPortString = portString.Trim('\n');
                    Port = int.Parse(serverPortString);
                    // file.WriteLine("Prolog MQI port: " + _port);
                }
            }
            else
            {
                var domainSocket = _process.StandardOutput.ReadLine();

                if (domainSocket == "")
                    throw new PrologLaunchError("no Unix Domain Socket found in stdout");
                if (domainSocket != null)
                    UnixDomainSocket = domainSocket.Trim('\n');
            }

            var passwordString = _process.StandardOutput.ReadLine();
            if (passwordString == "")
                throw new PrologLaunchError("no password found in stdout");
            
            if (passwordString != null)
            {
                Password = passwordString.Trim('\n');
                // file.WriteLine("Prolog MQI password: " + _password);
            }

            if (_mqiTraces is null) return;
            var prologThread = CreateThread();
            prologThread.Query("debug(mqi({self._mqi_traces}))");
        }


        public PrologThread CreateThread()
        {
            return new PrologThread(this);
        }

        public int? ProcessId()
        {
            return _process?.Id; // null propagation operator
        }
    }

    public class PrologLaunchError : Exception
    {
        public PrologLaunchError(string message) : base(message) { }
    }


    public class PrologThread
    {
        private readonly PrologMqi _prologServer;
        private Socket _socket;
        // private string _communicationThreadId;
        // private string _goalThreadId;
        // private int _heartbeatCount;
        private int? _serverProtocolMajor;
        private int? _serverProtocolMinor;

        public PrologThread(PrologMqi prologMqi)
        {
            _prologServer = prologMqi;
            _socket = null;
            // _communicationThreadId = null;
            // _goalThreadId = null;
            // _heartbeatCount = 0;
            _serverProtocolMajor = null;
            _serverProtocolMinor = null;

            Start();
        }
        
        private void Start()
        {
            // using StreamWriter file = new("output.txt", append: true);

            // file.WriteLine("Entering PrologThread: " + _prolog_server.process_id());

            if (_socket != null)
                return;

            if (_prologServer.ProcessId() is null)
                _prologServer.Start();

            Dictionary<string, int?> prologAddress = new();

            if (_prologServer.UnixDomainSocket != null)
            {
                prologAddress.Add(_prologServer.UnixDomainSocket, _prologServer.Port);
                _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            }
            else
            {
                prologAddress.Add("127.0.0.1", _prologServer.Port);
                _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            }

            // file.WriteLine("PrologMQI connecting to Prolog at: " +
            // prologAddress.First().Key + ":" + prologAddress.First().Value);

            var connectCount = 0;
            while (connectCount < 3)
            {
                try
                {
                    _socket.Connect(IPAddress.Parse(prologAddress.First().Key), prologAddress.First().Value ?? default);
                    break;
                }
                catch (SocketException)
                {
                    // file.WriteLine("Server not responding %s", prologAddress);
                    connectCount += 1;
                    Thread.Sleep(1);
                }
            }
            if (connectCount == 3)
                throw new SocketException((int)SocketError.ConnectionRefused);

            // Send the password as first message
            Send(_prologServer.Password ?? "Null Password");
            var result = Receive();

            using var doc = JsonDocument.Parse(result);
            var jsonResult = doc.RootElement;


            // file.WriteLine("\nPrologMQI received: " + jsonResult.GetProperty("functor"));

            if (jsonResult.GetProperty("functor").ToString() != "true")
                throw new PrologLaunchError($"Failed to accept password: {jsonResult}");
            
            // _communicationThreadId = jsonResult.GetProperty("args")[0][0][0].GetProperty("args")[0].ToString();
            // _goalThreadId = jsonResult.GetProperty("args")[0][0][0].GetProperty("args")[1].ToString();

            // file.WriteLine("PrologMQI server protocol: " + communication_thread_id + " " + goal_thread_id);

            if (jsonResult.GetProperty("args")[0][0].GetArrayLength() > 1)
            {
                var versionTerm = jsonResult.GetProperty("args")[0][0][1];
                _serverProtocolMajor = versionTerm.GetProperty("args")[0].GetInt32();
                _serverProtocolMinor = versionTerm.GetProperty("args")[1].GetInt32();
            }
            else
            {
                _serverProtocolMajor = 0;
                _serverProtocolMinor = 0;
            }

            // file.WriteLine("PrologMQI server protocol: " + _server_protocol_major + " " + _server_protocol_minor);

            CheckProtocolVersion();
        }

        private void CheckProtocolVersion()
        {
            const int requiredServerMajor = 1;
            const int requiredServerMinor = 0;

            switch (_serverProtocolMajor)
            {
                case 0 when _serverProtocolMinor == 0:
                case requiredServerMajor when _serverProtocolMinor == requiredServerMinor:
                    return;
                default:
                    throw new PrologLaunchError($"version of swiplserver requires MQI major version" +
                                                $" {requiredServerMajor} and minor version >= {requiredServerMinor}." +
                                                $" The server is running MQI '{_serverProtocolMajor}.{_serverProtocolMinor}");
            }
        }

        private string Receive()
        {
            // using StreamWriter file = new("output.txt", append: true);

            var buffer = new byte[4096];

            if (_socket is null)
                throw new NullReferenceException("Socket is null");

            var iRx = _socket.Receive(buffer);

            var msg = Encoding.ASCII.GetString(buffer, 0, iRx);

            // file.WriteLine("\nReceived: " + msg);

            msg = msg[(msg.IndexOf('.') + 1)..];

            return msg;
        }

        private void Send(string value)
        {
            // using StreamWriter file = new("output.txt", append: true);

            value = value.Trim();
            value = value.Trim('\n');
            value = value.Trim('.');
            value += ".\n";

            // file.WriteLine("PrologMQI send: " + value);

            var valueBytes = Encoding.UTF8.GetBytes(value);
            var utf8Value = Encoding.UTF8.GetString(valueBytes);

            // file.WriteLine("Utf8 Value: " + utf8value);
            
            var messageLen = _serverProtocolMajor == 0 ? value.Length : utf8Value.Length;

            var msgHeader = messageLen.ToString() + ".\n";
            var messageLenBytes = Encoding.UTF8.GetBytes(msgHeader);

            // file.Write("Message Len: ");
            // for (int i = 0; i < messageLenBytes.Length; i++)
            // file.Write(messageLenBytes[i]);


            // file.Write("\nMessage Content: ");
            // for (int i = 0; i < valueBytes.Length; i++)
            // file.Write(valueBytes[i]);

            if (_socket == null) return;
            _socket.Send(messageLenBytes);
            _socket.Send(valueBytes);
        }

        // private void Stop()
        // {
        //     if (_socket == null) return;
        //     if (!_prologServer.ConnectionFailed) return;
        //
        //     try
        //     {
        //         Send("close.\n");
        //         ReturnPrologResponse();
        //         
        //         _socket.Close();
        //     }
        //     catch (Exception)
        //     { // ignored
        //     }
        //
        //     _socket = null;
        // }


        public IEnumerable<string[]> Query(string value, float? queryTimeoutSeconds = null)
        {
            if (_socket is null)
                Start();

            value = value.Trim();
            value = value.Trim('\n');

            var timeoutString = "_"; 
            if (queryTimeoutSeconds is not null)
                timeoutString = queryTimeoutSeconds.ToString();

            Send($"run(({value}), {timeoutString}).\n");

            return ReturnPrologResponse();
        }

        public void QueryAsync(string value, bool findAll, float? queryTimeoutSeconds = null)
        {
            if (_socket is null)
                Start();

            value = value.Trim();
            value = value.Trim('\n');

            var timeoutString = queryTimeoutSeconds.ToString();
            if (queryTimeoutSeconds is null)
                timeoutString = "_";

            Send($"run_async(({value}), {timeoutString}, {findAll.ToString().ToLower()}).\n");

            ReturnPrologResponse();
        }

        public void CancelQueryAsync()
        {
            Send("cancel_async.\n");
            ReturnPrologResponse();
        }

        public List<string[]> QueryAsyncResult(float? waitTimeoutSeconds = null)
        {
            // StreamWriter file = new("output.txt", append: true);

            var timeoutString = waitTimeoutSeconds.ToString();
            if (waitTimeoutSeconds is null)
                timeoutString = "-1";

            // file.WriteLine("\nquery_async_result: " + timeoutString);

            Send($"async_result({timeoutString}).\n");

            return ReturnPrologResponse();
        }

        public void HaltServer()
        {
            Send("quit.\n");
            ReturnPrologResponse();
            _prologServer.ConnectionFailed = true;
        }

        private List<string[]> ReturnPrologResponse()
        {
            // using StreamWriter file = new("output.txt", append: true);
            var result = Receive();

            // file.WriteLine("\nReceive: " + result);

            List<string[]> answerList = new();


            using var doc = JsonDocument.Parse(result);
            var jsonResult = doc.RootElement;

            // file.WriteLine("Prolog Response:" + jsonResult);

            if (jsonResult.ToString() != "false" && jsonResult.GetProperty("functor").ToString() == "exception")
            {
                if (jsonResult.GetProperty("args")[0].ToString() == "no_more_results") return null;
                if (jsonResult.GetProperty("args")[0].ToString() == "connection_failed")
                    _prologServer.ConnectionFailed = true;
                // else if (!typeof(string).IsInstanceOfType(jsonResult.GetProperty("args")[0]))
                    // throw new PrologLaunchError(jsonResult.ToString());

                if (jsonResult.GetProperty("args")[0].ToString() == "connection_failed")
                    throw new PrologConnectionFailedError(jsonResult.GetProperty("args")[0].ToString());
                if (jsonResult.GetProperty("args")[0].ToString() == "time_limit_exceeded")
                    throw new PrologQueryTimeoutError(jsonResult.GetProperty("args")[0].ToString());
                if (jsonResult.GetProperty("args")[0].ToString() == "no_query")
                    throw new PrologNoQueryError(jsonResult.GetProperty("args")[0].ToString());
                if (jsonResult.GetProperty("args")[0].ToString() == "cancel_goal")
                    throw new PrologQueryCancelledError(jsonResult.GetProperty("args")[0].ToString());
                if (jsonResult.GetProperty("args")[0].ToString() == "result_not_available") throw new PrologResultNotAvailableError(jsonResult.GetProperty("args")[0].ToString());
            }
            else
            {
                if (jsonResult.ToString() == "false" || jsonResult.GetProperty("functor").ToString() == "false")
                    return null;

                // if (jsonResult.ToString() != "true" && jsonResult.GetProperty("functor").ToString() != "true")
                //     return null;

                for (var i = 0; i < jsonResult.GetProperty("args")[0].GetArrayLength(); i++)
                {
                    if (jsonResult.GetProperty("args")[0][i].GetArrayLength() == 0)
                        answerList.Add(new[] { "true", "null" });
                    else
                    {
                        for (var j = 0; j < jsonResult.GetProperty("args")[0][i].GetArrayLength(); j++)
                        {
                            answerList.Add(new[]
                            {
                                jsonResult.GetProperty("args")[0][i][j].GetProperty("args")[0].ToString(),
                                jsonResult.GetProperty("args")[0][i][j].GetProperty("args")[1].ToString()
                            });
                        }
                    }
                }

                if (answerList.Count > 0 && answerList.ElementAt(0)[0] == "true")
                    answerList.Add(new[] { "true", "null" });
            }
            return answerList;
        }
    }


    public static class PrologFunctions
    {
        public static string CreatePosixPath(string osPath)
        {
            return osPath.Replace("\\", "/");
        }

        // public static bool IsPrologFunctor(JsonElement jsonTerm)
        // {
        //     return jsonTerm.TryGetProperty("functor", out _) && jsonTerm.TryGetProperty("args", out _);
        // }

        private static bool IsPrologVariable(JsonElement jsonTerm)
        {
            return jsonTerm.GetProperty("args")[0].ToString() == "test" ||
                   jsonTerm[0].GetProperty("args")[0].ToString() == "_";
        }

        private static bool IsPrologAtom(JsonElement jsonTerm)
        {
            return !IsPrologVariable(jsonTerm);
        }

        public static JsonElement PrologName(JsonElement jsonTerm)
        {
            if (IsPrologAtom(jsonTerm) || IsPrologVariable(jsonTerm))
                return jsonTerm;
            
            return jsonTerm.GetProperty("functor");
        }

        public static JsonElement PrologArgs(JsonElement jsonTerm)
        {
            return jsonTerm.GetProperty("args");
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
        //     Convert json_term from the Prolog JSON format to a string that represents
        //      the term in the Prolog language. See `PrologThread.query` for documentation on the Prolog JSON format.
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


    public static class Test
    {
        public static void Main()
        {
            PrologMqi mqi = new();
            var prologThread = mqi.CreateThread();

            /*************************************************************************
            TEST 0 : Test of the consult command to use a prolog file 
            *************************************************************************/
            Console.WriteLine("TEST 0 : Consult command and query after consult\n");
            Console.WriteLine("Query: consult(test).");
            Console.WriteLine(prologThread.Query("consult(test)").ElementAt(0)[0]);
            Console.WriteLine("Query: father(bob).");
            Console.WriteLine(prologThread.Query("father(bob)").ElementAt(0)[0]);

            /*************************************************************************
            TEST 1 : Query with only one answer (i.e. true ou false)
            *************************************************************************/
            Console.WriteLine("\n\nTEST 1 : Query with only one answer (i.e. true ou false)\n");
            Console.WriteLine("Query: assertz(father(michael)).");
            Console.WriteLine(prologThread.Query("assertz(father(michael))").ElementAt(0)[0]);
            Console.WriteLine("\nQuery: father(michael).");
            Console.WriteLine(prologThread.Query("father(michael)").ElementAt(0)[0]);
            Console.WriteLine("\nQuery: father(paul).");
            Console.WriteLine(prologThread.Query("father(paul)").ElementAt(0)[0]);

            /*************************************************************************
            TEST 2 : Query with one atom and multiple answers
            *************************************************************************/
            Console.WriteLine("\n\nTEST 2 : Query with one argument and multiple answers\n");
            Console.WriteLine("Query: father(X)\n");
            prologThread.QueryAsync("father(X)", false);
            
            while (true)
            {
                var test2Result = prologThread.QueryAsyncResult();

                if (test2Result is null)
                {
                    Console.WriteLine("No more results");
                    break;
                }
                
                for (var i = 0; i < test2Result.Count; i++)
                {
                    Console.WriteLine(test2Result.ElementAt(i)[0] + " = " + test2Result.ElementAt(i)[1]);
                }
            }

            /*************************************************************************
            TEST 3 : Query with two arguments and multiple answers
            *************************************************************************/
            Console.WriteLine("\n\nTEST 3 : Query with two arguments and multiple answers\n");
            Console.WriteLine("Query: mother(X, Y)\n");
            prologThread.QueryAsync("mother(X, Y)", false);

            while (true)
            {
                var test3Result = prologThread.QueryAsyncResult();

                if (test3Result is null)
                {
                    Console.WriteLine("No more results");
                    break;
                }
                
                for (var i = 0; i < test3Result.Count; i++)
                {
                    Console.WriteLine(test3Result.ElementAt(i)[0] + " = " + test3Result.ElementAt(i)[1]);
                }
            }

            /*************************************************************************
            TEST 4 : Queries with threads
            *************************************************************************/
            Console.WriteLine("\n\nTEST 4: Queries with threads\n");
            Console.WriteLine("Query: prolog_thread2.query_async(sleep(1), father(michael))");
            Console.WriteLine("Query: prolog_thread1.query_async(father(kevin))");
            var prologThread1 = mqi.CreateThread();
            var prologThread2 = mqi.CreateThread();
            prologThread1.QueryAsync("sleep(1), father(michael)", false);
            prologThread2.QueryAsync("father(kevin)", false);

            Console.WriteLine("Thread 2: " + prologThread2.QueryAsyncResult().ElementAt(0)[0]);
            Console.WriteLine("Thread 1: " + prologThread1.QueryAsyncResult().ElementAt(0)[0]);

            /*************************************************************************
            TEST 5 : Query response time
            *************************************************************************/
            Console.WriteLine("\n\nTEST 5: Query response time\n");
            Console.WriteLine("Query: time(father(bob)).");
            Stopwatch timer = new();
            timer.Start();
            Console.WriteLine(prologThread.Query("time(father(bob))").ElementAt(0)[0]);
            Console.WriteLine("Time elapsed: {0}", timer.Elapsed);
            timer.Restart();
            Console.WriteLine(prologThread.Query("time(father(bob))").ElementAt(0)[0]);
            Console.WriteLine("Time elapsed: {0}", timer.Elapsed);


            /*************************************************************************
            TEST 6 : Answers with arrays
            *************************************************************************/
            Console.WriteLine("\n\nTEST 6: Answers with arrays\n");
            Console.WriteLine("Query: uncle([Col, Row], X, Y).");
            prologThread.QueryAsync("uncle([Col, Row], X, Y)", false);

            while (true)
            {
                var test6Result = prologThread.QueryAsyncResult();

                if (test6Result is null)
                {
                    Console.WriteLine("No more results");
                    break;
                }
                
                for (var i = 0; i < test6Result.Count; i++)
                {
                    Console.WriteLine(test6Result.ElementAt(i)[0] + " = " + test6Result.ElementAt(i)[1]);
                }
            }
        }
    }
}