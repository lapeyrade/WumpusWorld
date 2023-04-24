using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
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
        private PrologThread _prologThread;

        public void Awake()
        {
            _mqi = new PrologMqi(prologPath:"/opt/homebrew/bin/");
            _prologThread = _mqi.CreateThread();
            _prologThread.Query($"consult('{_prologFilePath}')");
        }

        protected void Update()
        {
            if (!askQuery) return; /* Prolog query inside Unity Inspector */
            _prologThread.QueryAsync(query, false);

            while (askQuery)
            {
                try
                {
                    var answer = _prologThread.QueryAsyncResult();
                    if (answer is null) askQuery = false;
                    else
                    {
                        var result = "";
                        for (var i = 0; i < answer.Count; i++)
                            result += answer.ElementAt(i)[0] + " = " + answer.ElementAt(i)[1] + "; ";
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

        public void UpdateKb()
        {
            _prologThread.Query("retractall(location(_, _))");
            _prologThread.Query("retractall(trait(_, _))");

            foreach (var agent in GameManager.Instance.agents)
            {
                _prologThread.Query($"assertz(location({agent.name}, [{agent.GetComponent<Agent.Agent>().coords.x}," +
                                    $" {agent.GetComponent<Agent.Agent>().coords.y}]))");

                foreach (var perso in GameManager.Instance.personalities.Where(perso =>
                             agent.GetComponent(Type.GetType("Ontology." + perso))))
                    _prologThread.Query($"assertz(trait({agent.name}, {perso.ToString().ToLower()}))");
            }

            for (var i = GameManager.Instance.gridMin.x; i < GameManager.Instance.gridMax.x; i++)
            {
                for (var j = GameManager.Instance.gridMin.y; j < GameManager.Instance.gridMax.y; j++)
                {
                    foreach (var element in GameManager.Instance.AgentsMap[i, j].Select(x => x.tag))
                        _prologThread.Query($"assertz(location({element.ToLower()}, [{i}, {j}]))");
                }
            }
        }

        public List<string> QueryKb(string agentName)
        {
            _prologThread.QueryAsync($"genAction({agentName}, Perso, Obj, Elem2, Act)", false);

            var results = new List<string>();
            var guiList = new List<string> {"UpperTopLeft", "UpperBotLeft", "UpperTopRight", "UpperBotRight",
                "LowerTopLeft", "LowerBotLeft", "LowerTopRight", "LowerBotRight"};

            foreach (var gui in guiList)
                GameObject.Find(gui).GetComponent<TextMeshProUGUI>().text = "";
            
            while (true)
            {
                var answer = _prologThread.QueryAsyncResult();
                if (answer is null) break;

                results.Add(answer.ElementAt(3)[1]);
                var explanation = $"<b>{agentName}</b>" + " is <b>" + answer.ElementAt(0)[1] +
                                  "</b> and wants to <b>" + answer.ElementAt(1)[1] + "</b> regarding the <b>" +
                                  answer.ElementAt(2)[1] + "</b> so he will <b>" + answer.ElementAt(3)[1] + "</b>.";
                
                foreach (var gui in guiList.Where(gui => GameObject.Find(gui).GetComponent<TextMeshProUGUI>().text == ""))
                {
                    GameObject.Find(gui).GetComponent<TextMeshProUGUI>().text = explanation;
                    break;
                }
            }
            
            Debug.Log(results.Aggregate("", (current, r) => current + r + ", "));
            return results;
        }
    }
}