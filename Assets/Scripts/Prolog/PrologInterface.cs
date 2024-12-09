using System;
using System.IO;
using System.Linq;
using TMPro;
using UnityEngine;

namespace Prolog
{
    public class PrologInterface : MonoBehaviour
    {
        private TMP_Dropdown _dropdown;
        private PrologMqi _mqi;
        private PrologThread _prologThread;
        public string debug_query = "";
        public bool askQuery;
        private readonly string _prologFilePath = Path.Combine(Application.streamingAssetsPath, "article.pl");
        public string QueryText = "";

        // Initialize the Prolog interface
        public void Awake()
        {
            _mqi = new PrologMqi(prologPath: "/opt/homebrew/bin/");
            _prologThread = _mqi.CreateThread();
            _prologThread.Query($"consult('{_prologFilePath}')");
            _dropdown = GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>();
        }

        // Ask a query to Prolog
        public void RunQuery()
        {
            // Remove first 2 characters (", ") if not empty
            if (QueryText.Length > 2)
                QueryText = QueryText.Remove(0, 2);
            _prologThread.Query(QueryText);
            QueryText = "";
        }

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
                        // Append to file the result of the query
                        // File.AppendAllText(Path.Combine(Application.streamingAssetsPath, "result.txt"), result + "\n");
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

        // Query the knowledge base for agent actions
        public string QueryKb(string agentName, Vector2Int agentCoords)
        {
            _prologThread.QueryAsync($"genAction([{agentName}, [{agentCoords.x}, {agentCoords.y}]], Perso, Obj, Elem2, Act, Uti)", false);

            var action = "";
            var maxUtil = 0;
            var chosenExplanation = "";

            _dropdown.ClearOptions();

            while (true)
            {
                var answer = _prologThread.QueryAsyncResult();
                if (answer is null)
                    break;

                var explanation = $"<b>{agentName}</b>" + " is <b>" + answer.ElementAt(0)[1] +
                                  "</b> and wanted to <b>" + answer.ElementAt(1)[1] + "</b> regarding the <b>" +
                                  answer.ElementAt(2)[1] + "</b> so one possible action was to <b>" + answer.ElementAt(3)[1] +
                                  "</b> with a utility of <b>" + answer.ElementAt(4)[1];

                if (Convert.ToInt32(answer.ElementAt(4)[1]) > maxUtil)
                {
                    action = answer.ElementAt(3)[1];
                    maxUtil = Convert.ToInt32(answer.ElementAt(4)[1]);
                    chosenExplanation = explanation;
                }
                _dropdown.options.Add(new TMP_Dropdown.OptionData(explanation));
            }
            _dropdown.captionText.text = chosenExplanation;

            return action;
        }
    }
}