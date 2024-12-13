using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

namespace Prolog
{
    /// <summary>
    /// Unity interface for SWI Prolog integration, allowing Prolog queries to be executed from within Unity
    /// as if Unity had a Prolog engine running inside of it. Queries are sent as strings and responses are
    /// handled as JSON.
    /// 
    /// This class provides:
    /// - Automatic management of starting and stopping a SWI Prolog instance
    /// - Interface to run Prolog queries through the Machine Query Interface (MQI)
    /// - Unity-specific functionality for integrating with GameObjects and UI
    /// 
    /// Installation Requirements:
    /// 1. Install SWI Prolog (www.swi-prolog.org) and ensure "swipl" is on the system path
    /// 2. Verify MQI is available by running "swipl mqi --help"
    /// </summary>
    public class PrologInterface : MonoBehaviour, IDisposable
    {
        private TMP_Dropdown _dropdown;
        private PrologMqi _mqi;
        private PrologThread _prologThread;
        private readonly List<PrologThread> _additionalThreads = new List<PrologThread>();
        private bool _isDisposed;

        public string debug_query = "";
        public bool askQuery;
        private readonly string _prologFilePath = Path.Combine(Application.streamingAssetsPath, "article.pl");
        public string QueryText = "";

        // Debug settings
        public bool IsDebugMode { get; set; }
        public int? DebugPort { get; set; }
        public string DebugPassword { get; set; }

        /// <summary>
        /// Initialize the Prolog interface by creating a new PrologMQI instance,
        /// starting a Prolog thread, and loading the Prolog knowledge base.
        /// </summary>
        public void Awake()
        {
            try
            {
                var options = new Dictionary<string, object>();
                if (IsDebugMode && DebugPort.HasValue)
                {
                    options["port"] = DebugPort.Value;
                    options["password"] = DebugPassword;
                    options["launch_mqi"] = false; // Connect to existing Prolog process
                }

                _mqi = new PrologMqi(prologPath: "/opt/homebrew/bin/");
                _prologThread = _mqi.CreateThread();

                if (!File.Exists(_prologFilePath))
                {
                    throw new FileNotFoundException($"Prolog file not found: {_prologFilePath}");
                }

                var result = _prologThread.Query($"consult('{_prologFilePath.Replace("\\", "/")}')");
                if (result == null)
                {
                    throw new Exception("Failed to load Prolog file");
                }

                _dropdown = GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>();
                Debug.Log("Prolog initialization successful");
            }
            catch (Exception e)
            {
                Debug.LogError($"Error initializing Prolog: {e.Message}");
                throw;
            }
        }

        /// <summary>
        /// Creates a new Prolog thread for concurrent query execution.
        /// Each thread runs queries independently.
        /// </summary>
        /// <returns>The created PrologThread instance</returns>
        public PrologThread CreateAdditionalThread()
        {
            var thread = _mqi.CreateThread();
            _additionalThreads.Add(thread);
            return thread;
        }

        /// <summary>
        /// Cancels any running async query on the main Prolog thread.
        /// </summary>
        public void CancelCurrentQuery()
        {
            try
            {
                _prologThread?.CancelQueryAsync();
                askQuery = false;
            }
            catch (PrologNoQueryError)
            {
                // No query running, ignore
            }
            catch (Exception e)
            {
                Debug.LogError($"Error cancelling query: {e.Message}");
            }
        }

        /// <summary>
        /// Execute a Prolog query. If QueryText starts with ", ", these characters are removed.
        /// The query is run on the same Prolog thread every time, emulating the Prolog top level.
        /// </summary>
        public void RunQuery()
        {
            try
            {
                // Remove first 2 characters (", ") if not empty
                if (QueryText.Length > 2)
                    QueryText = QueryText.Remove(0, 2);
                _prologThread.Query(QueryText);
                QueryText = "";
            }
            catch (Exception e)
            {
                Debug.LogError($"Error running query: {e.Message}");
            }
        }

        protected void OnDestroy()
        {
            Dispose(true);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (_isDisposed) return;

            if (disposing)
            {
                // Clean up managed resources
                foreach (var thread in _additionalThreads)
                {
                    try
                    {
                        thread?.Dispose();
                    }
                    catch (Exception e)
                    {
                        Debug.LogError($"Error disposing thread: {e.Message}");
                    }
                }
                _additionalThreads.Clear();

                try
                {
                    _prologThread?.Dispose();
                }
                catch (Exception e)
                {
                    Debug.LogError($"Error disposing MQI: {e.Message}");
                }
            }

            _isDisposed = true;
        }

        /// <summary>
        /// Update loop that handles asynchronous Prolog queries.
        /// When askQuery is true, it executes the debug_query and processes the results.
        /// Results can be:
        /// - null: Query failed or completed
        /// - list: Contains variable bindings for successful queries
        /// - error: Logged if no answer is found or other errors occur
        /// </summary>
        protected void Update()
        {
            if (!askQuery) return; /* Prolog query inside Unity Inspector */
            _prologThread.QueryAsync(debug_query, false);

            while (askQuery)
            {
                try
                {
                    var answer = _prologThread.QueryAsyncResult();
                    if (answer is null)
                        askQuery = false;
                    else
                    {
                        var result = "";
                        for (var i = 0; i < answer.Count; i++)
                        {
                            if (answer.ElementAt(i)[0] == "false")
                                Debug.LogError($"No answer found.");
                            else result += answer.ElementAt(i)[0] + " = " + answer.ElementAt(i)[1] + "; ";
                        }
                        Debug.Log(result);
                    }
                }
                catch (PrologNoQueryError e)
                {
                    Debug.LogError(e);
                    askQuery = false;
                    break;
                }
            }
        }

        /// <summary>
        /// Query the Prolog knowledge base for agent actions.
        /// This method:
        /// 1. Constructs a query using agent name and coordinates
        /// 2. Executes the query asynchronously
        /// 3. Processes all answers to find the action with highest utility
        /// 4. Populates dropdown with all possible actions and explanations
        /// 5. Returns the chosen action
        /// 
        /// The query format follows the pattern:
        /// genAction([AgentName, [X, Y]], Personality, Objective, Element, Action, Utility)
        /// 
        /// Returns:
        /// - string: The chosen action with highest utility
        /// - Empty string: If no valid actions are found
        /// </summary>
        public string QueryKb(string agentName, Vector2Int agentCoords)
        {
            var startTime = Time.realtimeSinceStartup;
            var query = $"genAction([{agentName}, [{agentCoords.x}, {agentCoords.y}]], Perso, Obj, Elem2, Act, Uti)";

            _prologThread.QueryAsync(query, false);

            var action = "";
            var maxUtil = 0;
            var chosenExplanation = "";
            var answerCount = 0;

            _dropdown.ClearOptions();

            while (true)
            {
                var answer = _prologThread.QueryAsyncResult();
                if (answer is null)
                    break;

                answerCount++;
                try
                {
                    // Extract values safely from the answer
                    string GetValue(string[] pair) => pair[1].Trim('\'', '"');

                    var personality = GetValue(answer[0]);
                    var objective = GetValue(answer[1]);
                    var element = GetValue(answer[2]);
                    var act = GetValue(answer[3]);
                    var utility = GetValue(answer[4]);

                    var explanation = $"<b>{agentName}</b> is <b>{personality}</b> and wanted to <b>{objective}</b> " +
                                    $"regarding the <b>{element}</b> so one possible action was to <b>{act}</b> " +
                                    $"with a utility of <b>{utility}</b>";

                    var utilityValue = Convert.ToInt32(utility);
                    if (utilityValue > maxUtil)
                    {
                        action = act;
                        maxUtil = utilityValue;
                        chosenExplanation = explanation;
                    }
                    _dropdown.options.Add(new TMP_Dropdown.OptionData(explanation));
                }
                catch (Exception e)
                {
                    Debug.LogError($"Error parsing answer: {e.Message}\nAnswer: {string.Join(", ", answer.Select(a => $"[{string.Join(", ", a)}]"))}");
                    continue;
                }
            }

            var timeTaken = (Time.realtimeSinceStartup - startTime) * 1000f;
            Debug.Log($"Query size: {query.Length} chars, Found {answerCount} possible actions, Query time: {timeTaken:F2}ms");
            _dropdown.captionText.text = chosenExplanation;

            return action;
        }
    }
}