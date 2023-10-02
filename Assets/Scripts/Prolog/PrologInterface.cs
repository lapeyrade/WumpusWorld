using System;
using System.IO;
using System.Linq;
using TMPro;
using UnityEngine;

namespace Prolog
{
    public class PrologInterface : MonoBehaviour
    {
        [SerializeField] public string query = "";
        [SerializeField] public bool askQuery;
        private TMP_Dropdown _dropdown;

        private readonly string _prologFilePath = Path.Combine(Application.streamingAssetsPath, "article.pl");
        private PrologMqi _mqi;
        public PrologThread PrologThread;

        // Initialize the Prolog interface
        public void Init()
        {
            _mqi = new PrologMqi(prologPath:"/opt/homebrew/bin/");
            PrologThread = _mqi.CreateThread();
            PrologThread.Query($"consult('{_prologFilePath}')");
            _dropdown = GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>();
        }

        // Ask a query to Prolog
        protected void Update()
        {
            if (!askQuery) return; /* Prolog query inside Unity Inspector */
            PrologThread.QueryAsync(query, false);
            
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
                        // Debug.Log(result);
                    }
                } catch (PrologNoQueryError e)
                {
                    Debug.LogError(e);
                    askQuery = false;
                    break;
                }
            }
        }

        // Update the knowledge base with the current state of the game
        public void UpdateKb()
        {
            foreach (var agent in GameManager.Instance.agents)
            {
                PrologThread.Query($"retract(location({agent.name}, _))");
                PrologThread.Query($"retractall(trait({agent.name}, _))");
                
                PrologThread.Query($"assertz(location({agent.name}, [{agent.GetComponent<Agent.Agent>().coords.x}," +
                                    $" {agent.GetComponent<Agent.Agent>().coords.y}]))");
                
                foreach (var perso in GameManager.Instance.personalities.Where(perso =>
                             agent.GetComponent(Type.GetType("Ontology." + perso))))
                    PrologThread.Query($"assertz(trait({agent.name}, {perso.ToString().ToLower()}))");
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