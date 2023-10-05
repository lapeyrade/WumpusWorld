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
        public string debug_query = "";
        public bool askQuery;
        private readonly string _prologFilePath = Path.Combine(Application.streamingAssetsPath, "article.pl");
        private PrologMqi _mqi;
        private PrologThread PrologThread;
        public string QueryText = "";


        // Initialize the Prolog interface
        public void Init()
        {
            _mqi = new PrologMqi(prologPath: "/opt/homebrew/bin/");
            PrologThread = _mqi.CreateThread();
            PrologThread.Query($"consult('{_prologFilePath}')");
            _dropdown = GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>();
        }


        public void RunQuery()
        {
            // Remove first 2 characters (", ") if not empty
            if (QueryText.Length > 2)
                QueryText = QueryText.Remove(0, 2);
            PrologThread.Query(QueryText);
            QueryText = "";
        }

        // Ask a query to Prolog
        protected void Update()
        {
            if (!askQuery) return; /* Prolog query inside Unity Inspector */
            PrologThread.QueryAsync(debug_query, false);

            while (askQuery)
            {
                try
                {
                    var answer = PrologThread.QueryAsyncResult();
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
        public string QueryKb(string agentName)
        {
            PrologThread.QueryAsync($"genAction({agentName}, Perso, Obj, Elem2, Act, Uti)", false);

            var action = "";
            var maxUtil = 0;
            var chosenExplanation = "";

            _dropdown.ClearOptions();

            while (true)
            {
                var answer = PrologThread.QueryAsyncResult();
                if (answer is null) break;

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