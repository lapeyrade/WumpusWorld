// SWI-Prolog Machine Query Interface (MQI) C# Implementation
// Based on:
// - SWI-Prolog MQI: https://github.com/SWI-Prolog/packages-mqi
// - Python swiplserver: Author: Eric Zinda (ericz@inductorsoftware.com)
//
// The Machine Query Interface (MQI) allows using SWI-Prolog as an embedded part of an application.
// This C# implementation enables SWI-Prolog queries to be executed from within C# applications
// as if C# had a Prolog engine running inside of it. Queries are sent as strings and responses
// are handled as JSON.

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
using System.Threading.Tasks;

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
    /// Helper class to read process output streams asynchronously
    /// </summary>
    internal class NonBlockingStreamReader : IDisposable
    {
        private readonly StreamReader _reader;
        private readonly Task _readerTask;
        private readonly CancellationTokenSource _cts;
        private bool _disposed;

        public NonBlockingStreamReader(StreamReader reader)
        {
            _reader = reader;
            _cts = new CancellationTokenSource();
            _readerTask = Task.Run(ReadOutput, _cts.Token);
        }

        private async Task ReadOutput()
        {
            try
            {
                while (!_cts.Token.IsCancellationRequested)
                {
                    var line = await _reader.ReadLineAsync();
                    if (line == null) break;
                    Trace.WriteLine($"Prolog: {line}");
                }
            }
            catch (Exception e)
            {
                Trace.TraceError($"Error reading Prolog output: {e.Message}");
            }
        }

        public void Stop()
        {
            if (_disposed) return;
            _cts.Cancel();
            try
            {
                _readerTask.Wait(1000); // Wait up to 1 second for task to complete
            }
            catch (Exception) { /* Ignore cleanup errors */ }
        }

        public void Dispose()
        {
            if (_disposed) return;
            Stop();
            _cts.Dispose();
            _reader.Dispose();
            _disposed = true;
        }
    }

    /// <summary>
    /// Helper class for handling Prolog protocol functions
    /// </summary>
    internal static class PrologProtocol
    {
        // Protocol version information
        public const int RequiredServerMajor = 1;
        public const int RequiredServerMinor = 0;

        public static string PrologName(JsonElement json)
        {
            if (!IsPrologFunctor(json))
                return json.ToString();
            return json.GetProperty("functor").GetString();
        }

        public static JsonElement[] PrologArgs(JsonElement json)
        {
            if (!IsPrologFunctor(json))
                return Array.Empty<JsonElement>();
            return json.GetProperty("args").EnumerateArray().ToArray();
        }

        public static bool IsPrologFunctor(JsonElement json)
        {
            return json.ValueKind == JsonValueKind.Object &&
                   json.TryGetProperty("functor", out var functor) &&
                   functor.ValueKind == JsonValueKind.String &&
                   json.TryGetProperty("args", out var args) &&
                   args.ValueKind == JsonValueKind.Array;
        }

        public static bool IsPrologList(JsonElement json)
        {
            return json.ValueKind == JsonValueKind.Array;
        }

        public static bool IsPrologVariable(string term)
        {
            return !string.IsNullOrEmpty(term) &&
                   (char.IsUpper(term[0]) || term[0] == '_');
        }

        public static bool IsPrologAtom(string term)
        {
            return !string.IsNullOrEmpty(term) && !IsPrologVariable(term);
        }

        public static string QuotePrologIdentifier(string identifier)
        {
            if (!IsPrologAtom(identifier)) return identifier;

            var mustQuote = IsPrologAtom(identifier) && (
                identifier.Length == 0 ||
                !char.IsLetter(identifier[0]) ||
                !identifier.Replace("_", "").All(char.IsLetterOrDigit)
            );

            return mustQuote ? $"'{identifier}'" : identifier;
        }

        public static string JsonToProlog(JsonElement json)
        {
            if (IsPrologFunctor(json))
            {
                var args = PrologArgs(json).Select(JsonToProlog);
                return $"{QuotePrologIdentifier(PrologName(json))}({string.Join(", ", args)})";
            }
            if (IsPrologList(json))
            {
                var items = json.EnumerateArray().Select(JsonToProlog);
                return $"[{string.Join(", ", items)}]";
            }
            return QuotePrologIdentifier(json.ToString());
        }
    }

    /// <summary>
    /// Manages a SWI Prolog process and provides communication through the Machine Query Interface (MQI).
    /// This class allows Prolog to be used "like a normal library" in C#, enabling Prolog queries to be 
    /// executed as if C# had a Prolog engine running inside of it.
    /// 
    /// Based on the SWI-Prolog MQI (https://github.com/SWI-Prolog/packages-mqi) and its Python implementation.
    /// The Machine Query Interface enables embedding SWI-Prolog in applications, with all communication done
    /// using protocols that only work on the same machine (localhost TCP/IP or Unix Domain Sockets).
    /// 
    /// Features:
    /// - Automatic management of SWI Prolog process lifecycle
    /// - Secure communication through localhost TCP/IP or Unix Domain Sockets
    /// - Support for multiple concurrent Prolog threads
    /// - Comprehensive error handling and debugging support
    /// 
    /// Installation Requirements:
    /// 1. Install SWI Prolog (www.swi-prolog.org) and ensure "swipl" is on the system path
    /// 2. Verify MQI is available by running "swipl mqi --help"
    /// 
    /// For more information about the Machine Query Interface, see:
    /// - MQI Documentation: https://www.swi-prolog.org/pldoc/man?section=mqi
    /// - MQI Source: https://github.com/SWI-Prolog/packages-mqi
    /// </summary>
    public class PrologMQI : IDisposable
    {
        // Protocol version information
        private int _serverProtocolMajor;
        private int _serverProtocolMinor;

        // Connection fields
        private int? _port;
        private string _password;
        private string _unixDomainSocket;
        private Socket _socket;
        private bool _disposed;
        private bool _connectionFailed;

        // Process management
        private Process _process;
        private NonBlockingStreamReader _stderrReader;
        private NonBlockingStreamReader _stdoutReader;

        // Configuration (readonly)
        private readonly float? _queryTimeout;
        private readonly int? _pendingConnections;
        private readonly string _outputFile;
        private readonly string _mqiTraces;
        private readonly bool _launchMqi;
        private readonly string _prologPath;
        private readonly string _prologPathArgs;
        private readonly object _lock = new object();

        /// <summary>
        /// Gets whether the connection to MQI has failed and further communication will likely hang
        /// </summary>
        public bool ConnectionFailed
        {
            get => _connectionFailed;
            set => _connectionFailed = value;
        }

        /// <summary>
        /// Gets the port used for TCP/IP communication
        /// </summary>
        public int? Port => _port;

        /// <summary>
        /// Gets the password used for MQI authentication
        /// </summary>
        public string Password => _password;

        /// <summary>
        /// Gets the Unix domain socket path if being used
        /// </summary>
        public string UnixDomainSocket => _unixDomainSocket;

        /// <summary>
        /// Gets the process ID of the Prolog process if it's running
        /// </summary>
        public int? ProcessId() => _process?.Id;

        /// <summary>
        /// Initialize a PrologMQI instance that manages a SWI Prolog process.
        /// </summary>
        /// <param name="launchMqi">True to launch a new Prolog process, False to connect to existing one</param>
        /// <param name="port">Port for TCP/IP communication (ignored if unixDomainSocket is set). 
        /// When launchMqi is True, null automatically picks an open port.
        /// When launchMqi is False, must match the port in mqi_start/1.</param>
        /// <param name="password">Password for MQI authentication. 
        /// When launchMqi is True, null generates a secure password.
        /// When launchMqi is False, must match the password in mqi_start/1.</param>
        /// <param name="unixDomainSocket">Unix domain socket path (null for TCP/IP).
        /// Not supported on Windows. Path must be under 92 bytes for portability.</param>
        /// <param name="queryTimeoutSeconds">Default timeout for queries in seconds (null for no timeout)</param>
        /// <param name="pendingConnectionCount">Number of pending connections allowed (default 5)</param>
        /// <param name="outputFileName">File to redirect Prolog output (null for logging)</param>
        /// <param name="mqiTraces">MQI trace level ("_" for all, "protocol", "query", or null)</param>
        /// <param name="prologPath">Path to swipl executable</param>
        /// <param name="prologPathArgs">Additional arguments for swipl</param>
        /// <exception cref="ArgumentException">If arguments are invalid</exception>
        public PrologMQI(
            bool launchMqi = true,
            int? port = null,
            string password = null,
            string unixDomainSocket = null,
            float? queryTimeoutSeconds = null,
            int? pendingConnectionCount = null,
            string outputFileName = null,
            string mqiTraces = null,
            string prologPath = null,
            string prologPathArgs = null)
        {
            _port = port;
            _password = password;
            _unixDomainSocket = unixDomainSocket;
            _process = null;
            _queryTimeout = queryTimeoutSeconds;
            _pendingConnections = pendingConnectionCount;
            _outputFile = outputFileName;
            _mqiTraces = mqiTraces;
            _launchMqi = launchMqi;
            _prologPath = prologPath;
            _prologPathArgs = prologPathArgs;
            _connectionFailed = false;

            ValidateArguments();
        }

        private void ValidateArguments()
        {
            if (_unixDomainSocket != null)
            {
                if (Environment.OSVersion.Platform == PlatformID.Win32NT)
                {
                    throw new ArgumentException("Unix Domain Sockets are not supported on Windows");
                }
                if (_port.HasValue)
                {
                    throw new ArgumentException("Must only provide one of: port or unixDomainSocket");
                }
            }

            if (!_launchMqi && _outputFile != null)
            {
                throw new ArgumentException("outputFile only works when launchMqi is True");
            }
        }

        public void Start()
        {
            if (!_launchMqi) return;

            var swiplPath = _prologPath != null ? Path.Join(_prologPath, "swipl") : "swipl";
            var launchArgs = new List<string> { "mqi", "--write_connection_values=true" };

            if (_pendingConnections.HasValue)
                launchArgs.Add($"--pending_connections={_pendingConnections}");
            if (_queryTimeout.HasValue)
                launchArgs.Add($"--query_timeout={_queryTimeout}");
            if (_password != null)
                launchArgs.Add($"--password={_password}");
            if (_outputFile != null)
            {
                var finalPath = CreatePosixPath(_outputFile);
                launchArgs.Add($"--write_output_to_file={finalPath}");
                UnityEngine.Debug.Log($"Writing all Prolog output to file: {finalPath}");
            }
            if (_port.HasValue)
                launchArgs.Add($"--port={_port}");
            if (_unixDomainSocket != null)
            {
                if (!string.IsNullOrEmpty(_unixDomainSocket))
                    launchArgs.Add($"--unix_domain_socket={_unixDomainSocket}");
                else
                    launchArgs.Add("--create_unix_domain_socket=true");
            }

            try
            {
                _process = new Process
                {
                    StartInfo = new ProcessStartInfo
                    {
                        FileName = swiplPath,
                        Arguments = string.Join(" ", launchArgs),
                        UseShellExecute = false,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        CreateNoWindow = true
                    }
                };

                if (!_process.Start())
                {
                    throw new PrologLaunchError("Failed to start Prolog process");
                }

                // Read connection information
                if (_unixDomainSocket == null)
                {
                    var portString = _process.StandardOutput.ReadLine();
                    if (string.IsNullOrEmpty(portString))
                        throw new PrologLaunchError("No port found in stdout");
                    _port = int.Parse(portString.Trim());
                }
                else
                {
                    var socketPath = _process.StandardOutput.ReadLine();
                    if (string.IsNullOrEmpty(socketPath))
                        throw new PrologLaunchError("No Unix Domain Socket found in stdout");
                    _unixDomainSocket = socketPath.Trim();
                }

                var passwordString = _process.StandardOutput.ReadLine();
                if (string.IsNullOrEmpty(passwordString))
                    throw new PrologLaunchError("No password found in stdout");
                _password = passwordString.Trim();

                // Set up output readers
                _stderrReader = new NonBlockingStreamReader(_process.StandardError);
                _stdoutReader = new NonBlockingStreamReader(_process.StandardOutput);

                if (_mqiTraces != null)
                {
                    using var thread = CreateThread();
                    thread.Query($"debug(mqi({_mqiTraces}))");
                }
            }
            catch (Exception e)
            {
                CleanupProcess();
                throw new PrologLaunchError($"Failed to launch Prolog: {e.Message}");
            }
        }

        /// <summary>
        /// Stop the Prolog process and clean up resources.
        /// If the process was launched by this instance (launchMqi=true), it will be shut down.
        /// Does nothing if launchMqi=false.
        /// </summary>
        /// <param name="kill">If true, forcefully terminate the process using Process.Kill().
        /// If false (default), attempt an orderly shutdown through MQI.
        /// Note: Kill=true will be used regardless of this parameter if ConnectionFailed is true.</param>
        public void Stop(bool kill = false)
        {
            if (_process == null) return;

            try
            {
                if (kill || _connectionFailed)
                {
                    Trace.WriteLine("Killing Prolog process...");
                    _process.Kill();
                    Trace.WriteLine("Killed Prolog process.");
                }
                else
                {
                    try
                    {
                        using var thread = CreateThread();
                        thread.HaltServer();
                    }
                    catch (Exception e)
                    {
                        Trace.TraceWarning($"Orderly shutdown failed, killing process: {e.Message}");
                        _process.Kill();
                    }
                }

                _process.WaitForExit(1000); // Wait up to 1 second for process to exit
                if (!_process.HasExited)
                {
                    Trace.TraceWarning("Process did not exit cleanly, forcing termination");
                    _process.Kill();
                }

                CleanupProcess();

                if (_unixDomainSocket != null)
                {
                    try
                    {
                        if (File.Exists(_unixDomainSocket))
                        {
                            File.Delete(_unixDomainSocket);
                        }
                    }
                    catch (Exception e)
                    {
                        Trace.TraceWarning($"Failed to delete Unix domain socket: {e.Message}");
                    }
                }
            }
            catch (Exception e)
            {
                Trace.TraceError($"Error stopping Prolog: {e.Message}");
                throw;
            }
            finally
            {
                _process = null;
            }
        }

        private void CleanupProcess()
        {
            try
            {
                _stderrReader?.Stop();
                _stdoutReader?.Stop();
                _process?.Close();
                _process?.Dispose();
            }
            catch (Exception e)
            {
                Trace.TraceError($"Error cleaning up Prolog process: {e.Message}");
            }
        }

        /// <summary>
        /// Create a new thread for running Prolog queries.
        /// Each thread runs independently and can execute queries concurrently with other threads.
        /// Queries within a single thread are executed sequentially.
        /// </summary>
        /// <returns>A new PrologThread instance</returns>
        public PrologThread CreateThread()
        {
            lock (_lock)
            {
                return new PrologThread(this);
            }
        }

        /// <summary>
        /// Convert a file path to POSIX format for Prolog compatibility
        /// </summary>
        private static string CreatePosixPath(string path)
        {
            return path.Replace("\\", "/");
        }

        /// <summary>
        /// Creates a non-predictable filename suitable for use as a Unix domain socket.
        /// </summary>
        /// <param name="directory">The directory to create the socket in. Must be:
        /// - Short enough that total path is under 92 bytes
        /// - Accessible only to the current user
        /// - Have proper permissions for file creation/deletion</param>
        /// <returns>Full path to the socket file</returns>
        public static string CreateUnixDomainSocketFile(string directory)
        {
            var filename = $"sock{Guid.NewGuid():N}";
            return Path.Combine(directory, filename);
        }

        /// <summary>
        /// Check protocol version compatibility.
        /// Major versions increment for breaking changes.
        /// Minor versions increment for non-breaking changes.
        /// Version 0.0 has a protocol bug but is supported for compatibility.
        /// </summary>
        private void CheckProtocolVersion()
        {
            if (_serverProtocolMajor == 0 && _serverProtocolMinor == 0)
                return;

            if (_serverProtocolMajor == PrologProtocol.RequiredServerMajor && _serverProtocolMinor >= PrologProtocol.RequiredServerMinor)
                return;

            throw new PrologLaunchError(
                $"This version requires MQI major version {PrologProtocol.RequiredServerMajor} and minor version >= {PrologProtocol.RequiredServerMinor}. " +
                $"The server is running MQI '{_serverProtocolMajor}.{_serverProtocolMinor}'.");
        }

        public void Dispose()
        {
            if (_disposed) return;
            Stop();
            _socket?.Dispose();
            _disposed = true;
        }

        /// <summary>
        /// Send a message to the Prolog process.
        /// The format of sent messages is: <stringByteLength>.\n<stringBytes>.\n
        /// For example: 7.\nhello.\n
        /// - stringByteLength is the number of bytes of the string to follow (including .\n)
        /// - stringBytes is the actual message string, must end with .\n
        /// - Character encoding is UTF-8
        /// </summary>
        private void Send(string value)
        {
            value = value.Trim();
            value = value.TrimEnd('\n', '.');
            value += ".\n";

            Trace.WriteLine($"PrologMQI send: {value}");

            var utf8Value = Encoding.UTF8.GetBytes(value);
            var messageLen = _serverProtocolMajor == 0 ? value.Length : utf8Value.Length;
            var msgHeader = $"{messageLen}.\n";
            var headerBytes = Encoding.UTF8.GetBytes(msgHeader);

            _socket.Send(headerBytes);
            _socket.Send(utf8Value);
        }

        /// <summary>
        /// Receive a message from the Prolog process.
        /// The format is identical to Send: <stringByteLength>.\n<stringBytes>.\n
        /// Heartbeats (the "." character) can be sent by some commands to ensure the client is still listening.
        /// These are discarded.
        /// </summary>
        private string Receive()
        {
            var amountReceived = 0;
            int? amountExpected = null;
            var bytesReceived = new List<byte>();
            var sizeBytes = new List<byte>();

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
            Trace.WriteLine($"PrologMQI receive: {finalValue}");
            return finalValue;
        }

        /// <summary>
        /// Handle a Prolog response, converting it to the appropriate format or throwing an exception.
        /// Returns:
        /// - false/0: False
        /// - true[[]]: True
        /// - true[[], [], ...]: [True, True, ...]
        /// - true[[...], [...], ...]: [{"var1": json}, {"var1": json}, ...]
        /// - exception(no_more_results): null
        /// 
        /// Throws:
        /// - PrologConnectionFailedError if the connection failed
        /// - PrologQueryTimeoutError if the query timed out
        /// - PrologNoQueryError if attempting to cancel with no query
        /// - PrologQueryCancelledError if a query was cancelled
        /// - PrologResultNotAvailableError if async result not available
        /// - PrologError for other exceptions
        /// </summary>
        private object ReturnPrologResponse()
        {
            var result = Receive();
            var doc = JsonDocument.Parse(result);
            var jsonResult = doc.RootElement.Clone();
            doc.Dispose();

            // If the root element is a string, return it directly
            if (jsonResult.ValueKind == JsonValueKind.String)
            {
                return jsonResult.GetString();
            }

            // If the root element is not an object with functor/args, return it as is
            if (!PrologProtocol.IsPrologFunctor(jsonResult))
            {
                return jsonResult;
            }

            if (PrologProtocol.PrologName(jsonResult) == "exception")
            {
                if (jsonResult.GetProperty("args")[0].GetString() == "no_more_results")
                    return null;

                if (jsonResult.GetProperty("args")[0].GetString() == "connection_failed")
                    ConnectionFailed = true;

                var exceptionJson = jsonResult.GetProperty("args")[0];
                if (exceptionJson.ValueKind != JsonValueKind.String)
                    throw new PrologError(result);

                var exceptionType = exceptionJson.GetString();
                switch (exceptionType)
                {
                    case "connection_failed":
                        throw new PrologConnectionFailedError(result);
                    case "time_limit_exceeded":
                        throw new PrologQueryTimeoutError(result);
                    case "no_query":
                        throw new PrologNoQueryError(result);
                    case "cancel_goal":
                        throw new PrologQueryCancelledError(result);
                    case "result_not_available":
                        throw new PrologResultNotAvailableError(result);
                    default:
                        throw new PrologError(result);
                }
            }

            if (PrologProtocol.PrologName(jsonResult) == "false")
                return false;

            if (PrologProtocol.PrologName(jsonResult) == "true")
            {
                var answers = new List<object>();
                foreach (var answer in PrologProtocol.PrologArgs(jsonResult)[0].EnumerateArray())
                {
                    if (answer.GetArrayLength() == 0)
                    {
                        answers.Add(true);
                    }
                    else
                    {
                        var dict = new Dictionary<string, JsonElement>();
                        foreach (var assignment in answer.EnumerateArray())
                        {
                            var args = PrologProtocol.PrologArgs(assignment);
                            dict[args[0].GetString()] = args[1].Clone();
                        }
                        answers.Add(dict);
                    }
                }
                return answers.Count == 1 && answers[0] is true ? true : answers;
            }

            return jsonResult.Clone();
        }
    }

    /// <summary>
    /// Represents a thread in Prolog (not a C# thread).
    /// A given PrologThread instance will always run queries on the same Prolog thread.
    /// </summary>
    public class PrologThread : IDisposable
    {
        private readonly PrologMQI _prologServer;
        private Socket _socket;
        private string _communicationThreadId;
        private string _goalThreadId;
        private int _heartbeatCount;
        private int? _serverProtocolMajor;
        private int? _serverProtocolMinor;
        private bool _disposed;

        public PrologThread(PrologMQI prologMqi)
        {
            _prologServer = prologMqi;
            _socket = null;
            _communicationThreadId = null;
            _goalThreadId = null;
            _heartbeatCount = 0;
            _serverProtocolMajor = null;
            _serverProtocolMinor = null;
            _disposed = false;

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
            if (_serverProtocolMajor == 0 && _serverProtocolMinor == 0)
                return;

            if (_serverProtocolMajor == PrologProtocol.RequiredServerMajor && _serverProtocolMinor >= PrologProtocol.RequiredServerMinor)
                return;

            throw new PrologLaunchError(
                $"This version requires MQI major version {PrologProtocol.RequiredServerMajor} and minor version >= {PrologProtocol.RequiredServerMinor}. " +
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
            if (_disposed) return;
            Stop();
            _disposed = true;
        }

        public IEnumerable<string[]> Query(string value, float? queryTimeoutSeconds = null)
        {
            if (_socket is null)
                Start();

            value = value.Trim();
            value = value.Trim('\n');

            var timeoutString = queryTimeoutSeconds?.ToString() ?? "_";

            Send($"run(({value}), {timeoutString}).\n");

            var result = ReturnPrologResponse();

            if (result == null) return Enumerable.Empty<string[]>();
            if (result is bool b) return new[] { new[] { b.ToString().ToLower(), "null" } };
            if (result is List<object> list)
            {
                return list.Select(item =>
                {
                    if (item is bool) return new[] { "true", "null" };
                    if (item is Dictionary<string, JsonElement> dict)
                    {
                        return dict.Select(kvp => new[] { kvp.Key, kvp.Value.ToString() }).First();
                    }
                    return new[] { item.ToString(), "null" };
                });
            }
            return Enumerable.Empty<string[]>();
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

        public object QueryAsyncResult(float? waitTimeoutSeconds = null)
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

        private object ReturnPrologResponse()
        {
            var result = Receive();
            var doc = JsonDocument.Parse(result);
            var jsonResult = doc.RootElement.Clone();
            doc.Dispose();

            // If the root element is a string, return it directly
            if (jsonResult.ValueKind == JsonValueKind.String)
            {
                return jsonResult.GetString();
            }

            // If the root element is not an object with functor/args, return it as is
            if (!PrologProtocol.IsPrologFunctor(jsonResult))
            {
                return jsonResult;
            }

            if (PrologProtocol.PrologName(jsonResult) == "exception")
            {
                if (jsonResult.GetProperty("args")[0].GetString() == "no_more_results")
                    return null;

                if (jsonResult.GetProperty("args")[0].GetString() == "connection_failed")
                    _prologServer.ConnectionFailed = true;

                var exceptionJson = jsonResult.GetProperty("args")[0];
                if (exceptionJson.ValueKind != JsonValueKind.String)
                    throw new PrologError(result);

                var exceptionType = exceptionJson.GetString();
                switch (exceptionType)
                {
                    case "connection_failed":
                        throw new PrologConnectionFailedError(result);
                    case "time_limit_exceeded":
                        throw new PrologQueryTimeoutError(result);
                    case "no_query":
                        throw new PrologNoQueryError(result);
                    case "cancel_goal":
                        throw new PrologQueryCancelledError(result);
                    case "result_not_available":
                        throw new PrologResultNotAvailableError(result);
                    default:
                        throw new PrologError(result);
                }
            }

            if (PrologProtocol.PrologName(jsonResult) == "false")
                return false;

            if (PrologProtocol.PrologName(jsonResult) == "true")
            {
                var answers = new List<object>();
                foreach (var answer in PrologProtocol.PrologArgs(jsonResult)[0].EnumerateArray())
                {
                    if (answer.GetArrayLength() == 0)
                    {
                        answers.Add(true);
                    }
                    else
                    {
                        var dict = new Dictionary<string, JsonElement>();
                        foreach (var assignment in answer.EnumerateArray())
                        {
                            var args = PrologProtocol.PrologArgs(assignment);
                            dict[args[0].GetString()] = args[1].Clone();
                        }
                        answers.Add(dict);
                    }
                }
                return answers.Count == 1 && answers[0] is true ? true : answers;
            }

            return jsonResult.Clone();
        }

        private void Send(string value)
        {
            value = value.Trim();
            value = value.TrimEnd('\n', '.');
            value += ".\n";

            Trace.WriteLine($"PrologMQI send: {value}");

            var utf8Value = Encoding.UTF8.GetBytes(value);
            var messageLen = _serverProtocolMajor == 0 ? value.Length : utf8Value.Length;

            var msgHeader = $"{messageLen}.\n";
            var headerBytes = Encoding.UTF8.GetBytes(msgHeader);

            _socket.Send(headerBytes);
            _socket.Send(utf8Value);
        }

        /// <summary>
        /// Receive a message from the Prolog process.
        /// The format is identical to Send: <stringByteLength>.\n<stringBytes>.\n
        /// Heartbeats (the "." character) can be sent by some commands to ensure the client is still listening.
        /// These are discarded.
        /// </summary>
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
            Trace.WriteLine($"PrologMQI receive: {finalValue}");
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