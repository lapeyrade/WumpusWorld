using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using PrologMachineQueryInterface;
using UnityEngine;

public class PrologInterface : MonoBehaviour
{
    [SerializeField] public bool debugFileLog = true;
    [SerializeField] public bool consoleLog;
    [SerializeField] public string query = "situation([X, Y], Z)";
    [SerializeField] public bool askQuery;

    private readonly string _prologFilePath = Path.Combine(Application.streamingAssetsPath, "kb.pl");
    private readonly string _debugPrologFilePath = Path.Combine(Application.streamingAssetsPath, "debugkb.pl");

    private PrologMqi _mqi;
    private PrologThread _prologThread;

    public void InitKnowledgeBase()
    {
        _mqi = new PrologMqi();
        _prologThread = _mqi.CreateThread();
        _prologThread.Query("consult('" + _prologFilePath + "')");
    }

    protected void Update()
    {
        /* Prolog query inside the Unity Inspector */
        if (askQuery)
        {
            _prologThread.QueryAsync(query, false);

            Debug.Log("Query: " + query);

            bool moreResults = true;

            while (moreResults)
            {
                foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
                {
                    if (answer.Item1 == "null" && answer.Item2 == "null")
                        moreResults = false;
                    else
                        Debug.Log(answer.Item1 + " = " + answer.Item2);
                }
            }
            askQuery = false;
        }
    }

    public void InitialiseAgents(List<Human> agents)
    {
        foreach (Human agent in agents)
        {
            AddToKb($"nb_gold({agent.agentName}, {0})", true);
            AddToKb($"nb_arrow({agent.agentName}, {agent.nbArrow})", true);
            AddToKb($"intelligence({agent.agentName}, {agent.intelligence})", true);
            AddToKb($"strength({agent.agentName}, {agent.strength})", true);
            AddToKb($"dexterity({agent.agentName}, {agent.dexterity})", true);
        }
    }

    public string NextMove(Human agent)
    {
        _prologThread.QueryAsync($"move({agent.agentName}, Move)", false);
        List<Tuple<string, string>> result = _prologThread.QueryAsyncResult();

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public string RandomMove(Human agent)
    {
        _prologThread.QueryAsync($"random_move({agent.agentName}, Move)", false);
        List<Tuple<string, string>> result = _prologThread.QueryAsyncResult();

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public string NextAction(Human agent)
    {
        _prologThread.QueryAsync($"action({agent.agentName}, Action)", false);
        List<Tuple<string, string>> result = _prologThread.QueryAsyncResult();

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public bool CheckCellElement((int x, int y) coord, string element)
    {
        return bool.Parse(_prologThread.Query($"is_true(situation([{coord.x}, {coord.y}], {element}))").ElementAt(0).Item1);
    }

    public List<Vector2Int> CheckElement(string element)
    {
        List<Vector2Int> listCoordElem = new();

        _prologThread.QueryAsync($"list_element([Col, Row], {element})", false);

        bool moreResults = true;

        while (moreResults)
        {
            Vector2Int coord = new(0, 0);
            bool colRetrieved = false;

            foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
            {
                if (answer.Item1 == "Col")
                {
                    coord.x = int.Parse(answer.Item2);
                    colRetrieved = true;
                }
                else if (answer.Item1 == "Row")
                {
                    coord.y = int.Parse(answer.Item2);
                    if (colRetrieved)
                        listCoordElem.Add(coord);
                }
                else if (answer.Item1 == "null" && answer.Item2 == "null")
                    moreResults = false;
            }
        }
        return listCoordElem;
    }


    /***************** KB I/O *****************/
    public void AddToKb(string predicate, bool verifyKb)
    {
        if (!verifyKb || (_prologThread.Query(predicate).ElementAt(0).Item1 == "false"))
            _prologThread.Query($"assertz({predicate})");
    }

    public void RemoveFromKb(string predicate)
    {
        _prologThread.Query($"retract({predicate})");
    }

    public void AddCellContentKb(Vector2Int coord, string cellContent)
    {
        AddToKb($"cell([{coord.x}, {coord.y}], {cellContent})", true);
    }

    public void RemoveCellContentKb(Vector2Int coord, string cellContent)
    {
        RemoveFromKb($"cell([{coord.x}, {coord.y}], {cellContent})");
    }

    public List<string> GetPersonalities(Human agent)
    {
        _prologThread.QueryAsync($"personality({agent.agentName}, Personality)", false);

        List<string> listPersonalities = new();
        bool moreResults = true;

        while (moreResults)
        {
            foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
            {
                if (answer.Item1 == "null" || answer.Item2 == "null")
                    moreResults = false;
                else
                    listPersonalities.Add(answer.Item2);
            }
        }

        return listPersonalities;
    }



    /***************** LOGS *****************/
    public void PrintKb(List<Human> agents)
    {
        if (debugFileLog)
            ResetDebugKb();

        // ClearLog();

        PrintGlobalVariables("nb_arrow", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_gold", debugFileLog, consoleLog);
        PrintGlobalVariables("intelligence", debugFileLog, consoleLog);
        PrintGlobalVariables("strength", debugFileLog, consoleLog);
        PrintGlobalVariables("dexterity", debugFileLog, consoleLog);
        PrintGlobalVariables("personality", debugFileLog, consoleLog);

        if (consoleLog)
            Debug.Log("------------------\n");
        if (debugFileLog)
            WriteInDebugKb("% ------------------");

        PrintAgentMovements(debugFileLog, consoleLog, agents);

        PrintCellContent(debugFileLog, consoleLog);

        void PrintAgentMovements(bool debugFile, bool logConsole, List<Human> listAgent)
        {
            foreach (Human agent in listAgent)
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
            _prologThread.QueryAsync($"cell([Col, Row], Element)", false);

            List<string> listCol = new();
            List<string> listRow = new();
            List<string> listElement = new();

            bool moreResults = true;

            while (moreResults)
            {
                foreach (Tuple<string, string> answer in _prologThread.QueryAsyncResult())
                {
                    if (answer.Item1 == "Col")
                        listCol.Add(answer.Item2);
                    else if (answer.Item1 == "Row")
                        listRow.Add(answer.Item2);
                    else if (answer.Item1 == "Element")
                        listElement.Add(answer.Item2);
                    else if (answer.Item1 == "null" && answer.Item2 == "null")
                        moreResults = false;
                }
            }

            if (listCol.Count() == listRow.Count() && listRow.Count() == listElement.Count())
            {
                for (int i = 0; i < listCol.Count(); i++)
                {
                    if (logConsole)
                        Debug.Log($"cell([{listCol[i]}, {listRow[i]}], {listElement[i]}).");
                    if (debugFile)
                        WriteInDebugKb($"cell([{listCol[i]}, {listRow[i]}], {listElement[i]}).");
                }
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

            if (listElements.Count() == listCharacteristics.Count())
            {
                for (int i = 0; i < listElements.Count(); i++)
                {
                    if (logConsole)
                        Debug.Log($"{variable}({listElements[i]}, {listCharacteristics[i]}).");
                    if (debugFile)
                        WriteInDebugKb($"{variable}({listElements[i]}, {listCharacteristics[i]}).");
                }
            }
        }
    }

#if UNITY_EDITOR
    public void ClearLog()
    {
        var assembly = Assembly.GetAssembly(typeof(UnityEditor.Editor));
        var type = assembly.GetType("UnityEditor.LogEntries");
        var method = type.GetMethod("Clear");
        if (method != null) method.Invoke(new object(), null);
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
