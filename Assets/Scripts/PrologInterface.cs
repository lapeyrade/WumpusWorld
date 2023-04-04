using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using PrologMQI;
using UnityEngine;

public class PrologInterface : MonoBehaviour
{
    [SerializeField] public bool debugFileLog = true;
    [SerializeField] public bool consoleLog;
    [SerializeField] public string query = "situation(Elem, [X, Y])";
    [SerializeField] public bool askQuery;

    private readonly string _prologFilePath = Path.Combine(Application.streamingAssetsPath, "kb.pl");
    private readonly string _debugPrologFilePath = Path.Combine(Application.streamingAssetsPath, "debugkb.pl");

    private PrologMqi _mqi;
    private PrologThread _prologThread;

    private List<string>[,] _prologMap;

    public void Start()
    {
        _mqi = new PrologMqi();
        _prologThread = _mqi.CreateThread();
        _prologThread.Query($"consult('{_prologFilePath}')");
        _prologMap = new List<string>[GameManager.Instance.gridMax.x, GameManager.Instance.gridMax.y];
    }

    protected void Update()
    {
        /* Prolog query inside the Unity Inspector */
        if (!askQuery) return;
        UpdateKb();
        
        _prologThread.QueryAsync(query, false);
        
        Debug.Log($"Query: {query}");
        
        var moreResults = true;
        
        while (moreResults)
        {
            foreach (var (item1, item2) in _prologThread.QueryAsyncResult())
            {
                if (item1 != "null" && item2 != "null")
                    Debug.Log($"{item1} = {item2}");
                else moreResults = false;
            }
            Debug.Log("-----");
        }

        askQuery = false;
    }

    private void UpdateKb()
    {
        RemoveFromKb("situation(_, _)");
        for (var i = GameManager.Instance.gridMin.x; i < GameManager.Instance.gridMax.x; i++)
        {
            for (var j = GameManager.Instance.gridMin.y; j < GameManager.Instance.gridMax.y; j++)
            {
                foreach (var elem in GameManager.Instance.AgentsMap[i, j])
                {
                    AddToKb($"situation({elem.tag}, [{i}, {j}])", true);
                }
            }
        }
    }
    
    public void InitialiseAgents(Agent.Agent agent)
    {
        AddToKb($"assertz(situation(human, [X, Y]):- situation({agent.name}, [X, Y]))", true);

        AddToKb($"nb_gold({agent.name}, {0})", true);
        AddToKb($"nb_arrow({agent.name}, {agent.nbArrow})", true);
        AddToKb($"intelligence({agent.name}, {agent.intelligence})", true);
        AddToKb($"strength({agent.name}, {agent.strength})", true);
        AddToKb($"dexterity({agent.name}, {agent.dexterity})", true);
    }

    public string NextMove(Agent.Agent agent)
    {
        _prologThread.QueryAsync($"move({agent.name}, Move)", false);
        List<Tuple<string, string>> result = _prologThread.QueryAsyncResult();

        return result.Count == 0 ? "Default" : result.First().Item2;
    }

    public string RandomMove(Agent.Agent agent)
    {
        _prologThread.QueryAsync($"random_move({agent.name}, Move)", false);
        List<Tuple<string, string>> result = _prologThread.QueryAsyncResult();

        return result.Count == 0 ? "Default" : result.First().Item2;
    }

    public string NextAction(Agent.Agent agent)
    {
        _prologThread.QueryAsync($"action({agent.name}, Action)", false);
        List<Tuple<string, string>> result = _prologThread.QueryAsyncResult();

        return result.Count == 0 ? "Default" : result.First().Item2;
    }

    public List<Vector2Int> CheckElement(string element)
    {
        List<Vector2Int> listCoordElem = new();

        _prologThread.QueryAsync($"list_element({element}, [X, Y])", false);

        bool moreResults = true;
        while (moreResults)
        {
            Vector2Int coord = new(0, 0);

            foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
            {
                if (answer.Item1 == "X")
                    coord.x = int.Parse(answer.Item2);
                else if (answer.Item1 == "Y")
                {
                    coord.y = int.Parse(answer.Item2);
                    listCoordElem.Add(coord);
                }
                else if (answer.Item1 == "null" && answer.Item2 == "null")
                    moreResults = false;
            }
        }
        return listCoordElem;
    }


    /***************** KB I/O *****************/
    private void AddToKb(string predicate, bool verifyKb)
    {
        if (!verifyKb || _prologThread.Query(predicate).ElementAt(0).Item1 == "false")
            _prologThread.Query($"assertz({predicate})");
    }

    private void RemoveFromKb(string predicate)
    {
        _prologThread.Query($"retract({predicate})");
    }

    public void AddCellContentKb(string content, Vector2Int coord)
    {
        AddToKb($"cell({content}, [{coord.x}, {coord.y}])", true);
    }

    public void RemoveCellContentKb(string content, Vector2Int coord)
    {
        RemoveFromKb($"cell({content}, [{coord.x}, {coord.y}])");
    }

    public List<string> GetPersonalities(Agent.Agent agent)
    {
        _prologThread.QueryAsync($"personality({agent.name}, Personality)", false);

        List<string> listPersonalities = new();

        bool moreResults = true;
        while (moreResults)
        {
            foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
            {
                if (answer.Item1 != "null" || answer.Item2 != "null")
                    listPersonalities.Add(answer.Item2);
                else moreResults = false;
            }
        }

        return listPersonalities;
    }

    /***************** LOGS *****************/
    public void PrintKb(List<Agent.Agent> agents)
    {
        if (debugFileLog)
            ResetDebugKb();

        // ClearLog();

        // PrintGlobalVariables("nb_arrow", debugFileLog, consoleLog);
        // PrintGlobalVariables("nb_gold", debugFileLog, consoleLog);
        // PrintGlobalVariables("intelligence", debugFileLog, consoleLog);
        // PrintGlobalVariables("strength", debugFileLog, consoleLog);
        // PrintGlobalVariables("dexterity", debugFileLog, consoleLog);
        // PrintGlobalVariables("personality", debugFileLog, consoleLog);

        PrintGlobalVariables("nb_arrow", debugFileLog, false);
        PrintGlobalVariables("nb_gold", debugFileLog, false);
        PrintGlobalVariables("intelligence", debugFileLog, false);
        PrintGlobalVariables("strength", debugFileLog, false);
        PrintGlobalVariables("dexterity", debugFileLog, false);
        PrintGlobalVariables("personality", debugFileLog, false);

        // if (consoleLog)
        // Debug.Log("------------------\n");
        if (debugFileLog)
            WriteInDebugKb("% ------------------");

        // PrintAgentMovements(debugFileLog, consoleLog, agents);
        PrintAgentMovements(debugFileLog, false, agents);

        // PrintCellContent(debugFileLog, consoleLog);
        PrintCellContent(debugFileLog, false);

        void PrintAgentMovements(bool debugFile, bool logConsole, List<Agent.Agent> listAgent)
        {
            foreach (Agent.Agent agent in listAgent)
            {
                string stackTrace = "% ";
                foreach (Vector2Int movement in agent.PastMovements)
                    stackTrace += movement.ToString();

                if (logConsole)
                    Debug.Log(stackTrace);
                if (debugFile)
                    WriteInDebugKb(stackTrace);
            }
        }

        void PrintCellContent(bool debugFile, bool logConsole)
        {
            _prologThread.QueryAsync($"cell(Element, [X, Y])", false);

            List<string> listX = new();
            List<string> listY = new();
            List<string> listElement = new();

            while (true)
            {
                foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
                {
                    if (answer.Item1 == "X")
                        listX.Add(answer.Item2);
                    else if (answer.Item1 == "Y")
                        listY.Add(answer.Item2);
                    else if (answer.Item1 == "Element")
                        listElement.Add(answer.Item2);
                }
                break;
            }

            if (listX.Count() != listY.Count() || listY.Count() != listElement.Count()) return;

            for (int i = 0; i < listX.Count(); i++)
            {
                if (logConsole)
                    Debug.Log($"cell({listElement[i]}, [{listX[i]}, {listY[i]}]).");
                if (debugFile)
                    WriteInDebugKb($"cell({listElement[i]}, [{listX[i]}, {listY[i]}]).");
            }
        }

        void PrintGlobalVariables(string variable, bool debugFile, bool logConsole)
        {
            _prologThread.QueryAsync($"{variable}(Element, Characteristic)", false);

            List<string> listElements = new();
            List<string> listCharacteristics = new();

            bool moreResults = true;
            while (moreResults)
            {
                foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
                {
                    if (answer.Item1 == "Element")
                        listElements.Add(answer.Item2);
                    else if (answer.Item1 == "Characteristic")
                        listCharacteristics.Add(answer.Item2);
                    else if (answer.Item1 == "null" && answer.Item2 == "null")
                        moreResults = false;
                }
            }

            if (listElements.Count != listCharacteristics.Count) return;
            
            for (int i = 0; i < listElements.Count; i++)
            {
                if (logConsole)
                    Debug.Log($"{variable}({listElements[i]}, {listCharacteristics[i]}).");
                if (debugFile)
                    WriteInDebugKb($"{variable}({listElements[i]}, {listCharacteristics[i]}).");
            }
        }
    }

#if UNITY_EDITOR
    public void ClearLog()
    {
        var assembly = Assembly.GetAssembly(typeof(UnityEditor.Editor));
        var type = assembly.GetType("UnityEditor.LogEntries");
        var method = type.GetMethod("Clear");
        method?.Invoke(new object(), null);
    }
#endif

    /***************** DEBUG FILE *****************/
    private void ResetDebugKb()
    {
        string newVersion = "";
        string stoppingLine = "% DYNAMIC PART OF THE KNOWLEDGE BASE";

        StreamReader streamReader = File.OpenText(_debugPrologFilePath);
        while (streamReader.ReadLine() is { } oldVersion)
        {
            if (!oldVersion.Contains(stoppingLine))
                newVersion += oldVersion + Environment.NewLine;
            else
            {
                newVersion += stoppingLine + Environment.NewLine + Environment.NewLine;
                break;
            }
        }
        streamReader.Close();
        File.WriteAllText(_debugPrologFilePath, newVersion);
    }

    private void WriteInDebugKb(string prologText)
    {
        if (!File.Exists(_debugPrologFilePath))
        {
            // Create a file to write to.
            using StreamWriter streamWriter = File.CreateText(_debugPrologFilePath);
            streamWriter.WriteLine(prologText);
        }
        else
        {
            // Append text to existing file
            using StreamWriter streamWriter = File.AppendText(_debugPrologFilePath);
            streamWriter.WriteLine(prologText);
        }
    }
}
