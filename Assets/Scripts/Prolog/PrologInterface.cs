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

        private readonly string _prologFilePath = Path.Combine(Application.streamingAssetsPath, "article.pl");

        private PrologMqi _mqi;
        public PrologThread PrologThread;

        public void Init()
        {
            _mqi = new PrologMqi(prologPath:"/opt/homebrew/bin/");
            PrologThread = _mqi.CreateThread();
            PrologThread.Query($"consult('{_prologFilePath}')");
        }

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
                            result += answer.ElementAt(i)[0] + " = " + answer.ElementAt(i)[1] + "; ";
                        Debug.Log(result);
                    }
                } catch (PrologNoQueryError e)
                {
                    Debug.LogError(e);
                    askQuery = false;
                    break;
                }
            }
        }

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

        public string QueryKb(string agentName)
        {
            PrologThread.QueryAsync($"genAction({agentName}, Perso, Obj, Elem2, Act, Uti)", false);
            
            var action = "";
            var maxUtil = 0;
            var chosenExplanation = "";
            
            GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>().ClearOptions();
            
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
                GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>().options.Add(new TMP_Dropdown.OptionData(explanation));
            }
            GameObject.Find("Dropdown").GetComponent<TMP_Dropdown>().captionText.text = chosenExplanation;
           
            return action;
        }
    }
}