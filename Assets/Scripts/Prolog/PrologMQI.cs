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
    /// <summary>
    /// Base class used for all exceptions raised by PrologMQI except for PrologLaunchError.
    /// Used directly when an exception is thrown by Prolog code itself, otherwise the subclass exceptions are used.
    /// </summary>
    public class PrologError : Exception
    {
        private readonly string _exceptionJson;

        public PrologError(string exceptionJson)
        {
            using JsonDocument doc = JsonDocument.Parse(exceptionJson);
            JsonElement jsonResult = doc.RootElement;
        
            Debug.Assert(jsonResult.GetProperty("functor").GetString() == "exception" &&
                jsonResult.GetProperty("args").GetArrayLength() == 1);
            _exceptionJson = jsonResult.GetProperty("args")[0].ToString();
        }

        public string Json()
        {
            return _exceptionJson;
        }

        private string Prolog()
        {
            return PrologFunctions.JsonToProlog(_exceptionJson);
        }

        public override string Message => Prolog();

        public bool IsPrologException(string termName)
        {
            using JsonDocument doc = JsonDocument.Parse(_exceptionJson);
            return PrologFunctions.PrologName(doc.RootElement).GetString() == termName;
        }
    }

    /// <summary>
    /// Raised when the SWI Prolog process was unable to be launched for any reason.
    /// This can include a version mismatch between the library and the server.
    /// </summary>
    public class PrologLaunchError : Exception
    {
        public PrologLaunchError(string message) : base(message) { }
    }

    /// <summary>
    /// Raised when a Prolog query times out when calling PrologThread.Query() or PrologThread.QueryAsync() with a timeout.
    /// </summary>
    public class PrologQueryTimeoutError : Exception
    {
        public PrologQueryTimeoutError(string jsonResult) : base(jsonResult) { }
    }

    /// <summary>
    /// Raised when the connection used by a PrologThread fails.
    /// Indicates that the Machine Query Interface will no longer respond.
    /// </summary>
    public class PrologConnectionFailedError : Exception
    {
        public PrologConnectionFailedError(string jsonResult) : base(jsonResult) { }
    }

    /// <summary>
    /// Raised by PrologThread.CancelQueryAsync() and PrologThread.QueryAsyncResult() if there is no query running and no results to retrieve.
    /// </summary>
    public class PrologNoQueryError : Exception
    {
        public PrologNoQueryError(string jsonResult) : base(jsonResult) { }
    }

    /// <summary>
    /// Raised by PrologThread.QueryAsyncResult() when the query has been cancelled.
    /// </summary>
    public class PrologQueryCancelledError : Exception
    {
        public PrologQueryCancelledError(string jsonResult) : base(jsonResult) { }
    }

    /// <summary>
    /// Raised by PrologThread.QueryAsyncResult() when the next result to a query is not yet available.
    /// </summary>
    public class PrologResultNotAvailableError : Exception
    {
        public PrologResultNotAvailableError(string jsonResult) : base(jsonResult) { }
    }

    /// <summary>
    /// Manages a SWI Prolog process associated with your application process.
    /// This class is designed to allow Prolog to be used "like a normal library" using the Machine Query Interface of SWI Prolog.
    /// All communication is done using protocols that only work on the same machine as your application.
    /// </summary>
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

        /// <summary>
        /// Initializes a new instance of the PrologMqi class that manages a SWI Prolog process.
        /// </summary>
        /// <param name="launchMqi">If true (default), launches a SWI Prolog process. If false, connects to an existing process.</param>
        /// <param name="port">The TCP/IP localhost port to use for communication. If null, automatically picks an open port.</param>
        /// <param name="password">The password for authentication with the Prolog process.</param>
        /// <param name="unixDomainSocket">Unix domain socket path for communication. Not supported on Windows.</param>
        /// <param name="queryTimeoutSeconds">Default timeout for queries in seconds.</param>
        /// <param name="pendingConnectionCount">Number of pending connections to allow.</param>
        /// <param name="outputFileName">File to write Prolog output to.</param>
        /// <param name="prologPath">Path to the Prolog executable.</param>
        /// <param name="prologPathArgs">Additional arguments for the Prolog executable.</param>
        /// <param name="mqiTraces">Traces to enable for debugging.</param>
        public PrologMqi(bool launchMqi = true, int? port = null!, string password = null, string unixDomainSocket = null,
            float? queryTimeoutSeconds = null, int? pendingConnectionCount = null, string outputFileName = null,
            string prologPath = null, string prologPathArgs = null, string mqiTraces = null)
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
                if (Port != null)
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

    /// <summary>
    /// Represents a thread in Prolog (not a C# thread).
    /// A given PrologThread instance will always run queries on the same Prolog thread.
    /// </summary>
    public class PrologThread : IDisposable
    {
        private readonly PrologMqi _prologServer;
        private Socket _socket;
        private string _communicationThreadId;
        private string _goalThreadId;
        private int _heartbeatCount;
        private int? _serverProtocolMajor;
        private int? _serverProtocolMinor;

        public PrologThread(PrologMqi prologMqi)
        {
            _prologServer = prologMqi;
            _socket = null;
            _communicationThreadId = null;
            _goalThreadId = null;
            _heartbeatCount = 0;
            _serverProtocolMajor = null;
            _serverProtocolMinor = null;

            Start();
        }

        public void Start()
        {
            if (_socket != null)
                return;

            if (_prologServer.ProcessId() is null)
                _prologServer.Start();

            if (_prologServer.UnixDomainSocket != null)
            {
                _socket = new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.IP);
            }
            else
            {
                _socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            }

            var connectCount = 0;
            while (connectCount < 3)
            {
                try
                {
                    if (_prologServer.UnixDomainSocket != null)
                    {
                        _socket.Connect(new UnixDomainSocketEndPoint(_prologServer.UnixDomainSocket));
                    }
                    else
                    {
                        _socket.Connect(IPAddress.Parse("127.0.0.1"), _prologServer.Port ?? default);
                    }
                    break;
                }
                catch (SocketException)
                {
                    connectCount += 1;
                    Thread.Sleep(1000);
                }
            }
            if (connectCount == 3)
                throw new SocketException((int)SocketError.ConnectionRefused);

            // Send the password as first message
            Send(_prologServer.Password ?? "Null Password");
            var result = Receive();

            using var doc = JsonDocument.Parse(result);
            var jsonResult = doc.RootElement;

            if (jsonResult.GetProperty("functor").GetString() != "true")
                throw new PrologLaunchError($"Failed to accept password: {jsonResult}");

            var threadTerm = jsonResult.GetProperty("args")[0][0][0];
            _communicationThreadId = threadTerm.GetProperty("args")[0].GetString();
            _goalThreadId = threadTerm.GetProperty("args")[1].GetString();

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

            CheckProtocolVersion();
        }

        private void CheckProtocolVersion()
        {
            const int requiredServerMajor = 1;
            const int requiredServerMinor = 0;

            if (_serverProtocolMajor == 0 && _serverProtocolMinor == 0)
                return;

            if (_serverProtocolMajor == requiredServerMajor && _serverProtocolMinor >= requiredServerMinor)
                return;

            throw new PrologLaunchError(
                $"This version requires MQI major version {requiredServerMajor} and minor version >= {requiredServerMinor}. " +
                $"The server is running MQI '{_serverProtocolMajor}.{_serverProtocolMinor}'");
        }

        public void Stop()
        {
            if (_socket == null) return;
            if (!_prologServer.ConnectionFailed)
            {
                try
                {
                    Send("close.\n");
                    ReturnPrologResponse();
                }
                catch
                {
                    // Ignore exceptions during shutdown
                }
            }

            try
            {
                _socket.Close();
            }
            catch
            {
                // Ignore exceptions during shutdown
            }

            _socket = null;
        }

        public void Dispose()
        {
            Stop();
        }

        public IEnumerable<string[]> Query(string value, float? queryTimeoutSeconds = null)
        {
            if (_socket is null)
                Start();

            value = value.Trim();
            value = value.Trim('\n');

            var timeoutString = queryTimeoutSeconds?.ToString() ?? "_";

            Send($"run(({value}), {timeoutString}).\n");

            return ReturnPrologResponse();
        }

        public void QueryAsync(string value, bool findAll = true, float? queryTimeoutSeconds = null)
        {
            if (_socket is null)
                Start();

            value = value.Trim();
            value = value.Trim('\n');

            var timeoutString = queryTimeoutSeconds?.ToString() ?? "_";
            var findAllString = findAll.ToString().ToLower();

            Send($"run_async(({value}), {timeoutString}, {findAllString}).\n");

            ReturnPrologResponse();
        }

        public void CancelQueryAsync()
        {
            Send("cancel_async.\n");
            ReturnPrologResponse();
        }

        public List<string[]> QueryAsyncResult(float? waitTimeoutSeconds = null)
        {
            var timeoutString = waitTimeoutSeconds?.ToString() ?? "-1";

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
            var result = Receive();
            List<string[]> answerList = new();

            using var doc = JsonDocument.Parse(result);
            var jsonResult = doc.RootElement;

            if (jsonResult.GetProperty("functor").GetString() == "exception")
            {
                var exceptionType = jsonResult.GetProperty("args")[0].GetString();
                if (exceptionType == "no_more_results")
                    return null;
                if (exceptionType == "connection_failed")
                    _prologServer.ConnectionFailed = true;

                switch (exceptionType)
                {
                    case "connection_failed":
                        throw new PrologConnectionFailedError(jsonResult.ToString());
                    case "time_limit_exceeded":
                        throw new PrologQueryTimeoutError(jsonResult.ToString());
                    case "no_query":
                        throw new PrologNoQueryError(jsonResult.ToString());
                    case "cancel_goal":
                        throw new PrologQueryCancelledError(jsonResult.ToString());
                    case "result_not_available":
                        throw new PrologResultNotAvailableError(jsonResult.ToString());
                    default:
                        throw new PrologError(jsonResult.ToString());
                }
            }

            if (jsonResult.GetProperty("functor").GetString() == "false")
                return new List<string[]> { new[] { "false", "null" } };

            var answers = jsonResult.GetProperty("args")[0];
            for (var i = 0; i < answers.GetArrayLength(); i++)
            {
                var answer = answers[i];
                if (answer.GetArrayLength() == 0)
                {
                    answerList.Add(new[] { "true", "null" });
                }
                else
                {
                    for (var j = 0; j < answer.GetArrayLength(); j++)
                    {
                        var assignment = answer[j];
                        var args = assignment.GetProperty("args");
                        var variable = GetJsonValue(args[0]);
                        var value = GetJsonValue(args[1]);
                        answerList.Add(new[] { variable, value });
                    }
                }
            }

            if (answerList.Count > 0 && answerList[0][0] == "true")
                answerList.Add(new[] { "true", "null" });

            return answerList;
        }

        private string GetJsonValue(JsonElement element)
        {
            switch (element.ValueKind)
            {
                case JsonValueKind.String:
                    return element.GetString();
                case JsonValueKind.Array:
                    return $"[{string.Join(", ", element.EnumerateArray().Select(GetJsonValue))}]";
                case JsonValueKind.Object:
                    if (element.TryGetProperty("functor", out var functor) && element.TryGetProperty("args", out var args))
                    {
                        var funcName = functor.GetString();
                        var arguments = string.Join(", ", args.EnumerateArray().Select(GetJsonValue));
                        return $"{funcName}({arguments})";
                    }
                    return element.ToString();
                default:
                    return element.ToString();
            }
        }

        private void Send(string value)
        {
            value = value.Trim();
            value = value.Trim('\n', '.');
            value += ".\n";

            Debug.WriteLine($"PrologMQI send: {value}");

            var utf8Value = Encoding.UTF8.GetBytes(value);
            var messageLen = _serverProtocolMajor == 0 ? value.Length : utf8Value.Length;

            var msgHeader = $"{messageLen}.\n";
            var headerBytes = Encoding.UTF8.GetBytes(msgHeader);

            _socket.Send(headerBytes);
            _socket.Send(utf8Value);
        }

        private string Receive()
        {
            var amountReceived = 0;
            int? amountExpected = null;
            var bytesReceived = new List<byte>();
            var sizeBytes = new List<byte>();
            _heartbeatCount = 0;

            byte[] data = null;
            while (amountExpected is null || amountReceived < amountExpected)
            {
                var buffer = new byte[4096];
                var headerData = _socket.Receive(buffer);

                if (amountExpected is null)
                {
                    // Start / continue reading the string length
                    // Ignore any leading "." characters because those are heartbeats
                    for (var i = 0; i < headerData; i++)
                    {
                        var item = buffer[i];
                        // String length ends with '.\n' characters
                        if ((char)item == '.')
                        {
                            // ignore "."
                            if (sizeBytes.Count == 0)
                            {
                                // Count heartbeats for testing only
                                _heartbeatCount++;
                            }
                            continue;
                        }
                        if ((char)item == '\n')
                        {
                            // convert all the characters we've received so far to a number
                            var stringLength = Encoding.UTF8.GetString(sizeBytes.ToArray());
                            amountExpected = int.Parse(stringLength);
                            // And consume the rest of the stream
                            data = buffer.Skip(i + 1).Take(headerData - (i + 1)).ToArray();
                            break;
                        }
                        else
                        {
                            sizeBytes.Add(item);
                        }
                    }
                    if (data == null)
                        continue;
                }
                else
                {
                    data = buffer.Take(headerData).ToArray();
                }

                amountReceived += data.Length;
                bytesReceived.AddRange(data);
            }

            var finalValue = Encoding.UTF8.GetString(bytesReceived.ToArray());
            Debug.WriteLine($"PrologMQI receive: {finalValue}");
            return finalValue;
        }
    }


    /// <summary>
    /// Provides utility functions for working with Prolog terms and JSON.
    /// </summary>
    public static class PrologFunctions
    {
        public static string CreatePosixPath(string osPath)
        {
            return osPath.Replace("\\", "/");
        }

        public static bool IsPrologFunctor(JsonElement jsonTerm)
        {
            return jsonTerm.TryGetProperty("functor", out _) && jsonTerm.TryGetProperty("args", out _);
        }

        private static bool IsPrologVariable(JsonElement jsonTerm)
        {
            try
            {
                return jsonTerm.GetProperty("args")[0].GetString() == "test" ||
                       jsonTerm.GetProperty("args")[0].GetString() == "_";
            }
            catch
            {
                return false;
            }
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

        private static string QuotePrologIdentifier(string identifier)
        {
            if (!IsPrologAtom(JsonDocument.Parse($"{{\"{identifier}\"}}").RootElement))
                return identifier;

            bool mustQuote = identifier.Length == 0 ||
                            !char.IsLetter(identifier[0]) ||
                            !identifier.Replace("_", "").All(char.IsLetterOrDigit);

            return mustQuote ? $"'{identifier}'" : identifier;
        }

        public static string JsonToProlog(string jsonTerm)
        {
            using var doc = JsonDocument.Parse(jsonTerm);
            return JsonElementToProlog(doc.RootElement);
        }

        private static string JsonElementToProlog(JsonElement jsonTerm)
        {
            if (IsPrologFunctor(jsonTerm))
            {
                var args = PrologArgs(jsonTerm);
                var argStrings = new List<string>();
                foreach (var arg in args.EnumerateArray())
                {
                    argStrings.Add(JsonElementToProlog(arg));
                }
                return $"{QuotePrologIdentifier(PrologName(jsonTerm).GetString())}({string.Join(", ", argStrings)})";
            }
            else if (jsonTerm.ValueKind == JsonValueKind.Array)
            {
                var listStrings = new List<string>();
                foreach (var item in jsonTerm.EnumerateArray())
                {
                    listStrings.Add(JsonElementToProlog(item));
                }
                return $"[{string.Join(", ", listStrings)}]";
            }
            else
            {
                // Must be an atom, number or variable
                return QuotePrologIdentifier(jsonTerm.ToString());
            }
        }
    }
}