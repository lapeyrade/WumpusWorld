using System;
using System.IO;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using System.Text.Json;

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
        private PrologMQI _mqi;
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

                _mqi = new PrologMQI(prologPath: "/opt/homebrew/bin/");
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
                // Append query text into result.txt
                File.AppendAllText("result.txt", QueryText + ",");
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
            _prologThread.QueryAsync(debug_query, true);

            while (askQuery)
            {
                try
                {
                    var answer = _prologThread.QueryAsyncResult(0.1f);
                    if (answer is null)
                    {
                        askQuery = false;
                        break;
                    }
                    
                    if (answer is List<object> list)
                    {
                        foreach (var item in list)
                        {
                            if (item is Dictionary<string, JsonElement> dict)
                            {
                                // Build a complete answer string from all key-value pairs
                                var pairs = new List<string>();
                                foreach (var kvp in dict)
                                {
                                    pairs.Add($"{kvp.Key} = {kvp.Value}");
                                }
                                var result = string.Join(", ", pairs);
                                if (!string.IsNullOrEmpty(result))
                                {
                                    Debug.Log($"Answer: {result}");
                                }
                            }
                            else if (item is bool b)
                            {
                                Debug.Log($"Answer: {(b ? "true" : "false")}");
                            }
                            else
                            {
                                Debug.Log($"Answer: {item}");
                            }
                        }
                    }
                    else if (answer is string str)
                    {
                        Debug.Log($"Answer: {str}");
                    }
                    else if (answer is JsonElement jsonElement)
                    {
                        Debug.Log($"Answer: {jsonElement}");
                    }
                }
                catch (PrologNoQueryError)
                {
                    askQuery = false;
                    break;
                }
                catch (PrologResultNotAvailableError)
                {
                    // Result not ready yet, continue waiting
                    continue;
                }
                catch (Exception e)
                {
                    Debug.LogError($"Error in query: {e}");
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
            var socketStartTime = Time.realtimeSinceStartup;
            var query = $"genAction([{agentName}, [{agentCoords.x}, {agentCoords.y}]], Perso, Obj, Elem2, Act, Uti)";

            _prologThread.QueryAsync(query, false);

            var action = "";
            var maxUtil = 0;
            var chosenExplanation = "";
            var answerCount = 0;
            var prologStartTime = Time.realtimeSinceStartup;

            _dropdown.ClearOptions();

            while (true)
            {
                object answer;
                try
                {
                    answer = _prologThread.QueryAsyncResult();
                    if (answer is null)
                        break;

                    answerCount++;
                    if (answer is List<object> list && list.Count > 0 && list[0] is Dictionary<string, JsonElement> dict)
                    {
                        string GetValue(string key) => dict[key].ToString().Trim('\'', '"');

                        var personality = GetValue("Perso");
                        var objective = GetValue("Obj");
                        var element = GetValue("Elem2");
                        var act = GetValue("Act");
                        var utility = GetValue("Uti");

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
                }
                catch (Exception e)
                {
                    Debug.LogError($"Error parsing answer: {e.Message}");
                    continue;
                }
            }

            var socketTime = (prologStartTime - socketStartTime) * 1000f;
            var prologTime = (Time.realtimeSinceStartup - prologStartTime) * 1000f;
            Debug.Log($"Found {answerCount} possible actions, Socket time: {socketTime:F2}ms, Prolog inference time: {prologTime:F2}ms");
            _dropdown.captionText.text = chosenExplanation;

            return action;
        }
    }
}