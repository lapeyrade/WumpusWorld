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
    [SerializeField] public bool consoleLog = false;
    [SerializeField] public string query = "situation([X, Y], Z)";
    [SerializeField] public bool askQuery = false;

    private readonly string prologFilePath = Path.Combine(Application.streamingAssetsPath, "kb.pl");
    private readonly string debugPrologFilePath = Path.Combine(Application.streamingAssetsPath, "debugkb.pl");

    private PrologMQI mqi;

    private PrologThread prologThread;

    public void InitKnowledgeBase()
    {
        mqi = new PrologMQI();
        prologThread = mqi.CreateThread();
        prologThread.Query("consult('" + prologFilePath + "')");
    }

    protected void Update()
    {
        /* Prolog query inside the Unity Inspector */
        if (askQuery)
        {
            prologThread.QueryAsync(query, false);

            Debug.Log("Query: " + query);

            bool more_results = true;

            while (more_results)
            {
                foreach (Tuple<string, string> answer in prologThread.QueryAsyncResult())
                {
                    if (answer.Item1 == "null" && answer.Item2 == "null")
                        more_results = false;
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
            AddToKB($"nb_gold({agent.agentName}, {0})", true);
            AddToKB($"nb_arrow({agent.agentName}, {agent.nbArrow})", true);
            AddToKB($"intelligence({agent.agentName}, {agent.intelligence})", true);
            AddToKB($"strength({agent.agentName}, {agent.strength})", true);
            AddToKB($"dexterity({agent.agentName}, {agent.dexterity})", true);
        }
    }

    public string NextMove(Human agent)
    {
        prologThread.QueryAsync($"move({agent.agentName}, Move)", false);
        List<Tuple<string, string>> result = prologThread.QueryAsyncResult();

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public string RandomMove(Human agent)
    {
        prologThread.QueryAsync($"random_move({agent.agentName}, Move)", false);
        List<Tuple<string, string>> result = prologThread.QueryAsyncResult();

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public string NextAction(Human agent)
    {
        prologThread.QueryAsync($"action({agent.agentName}, Action)", false);
        List<Tuple<string, string>> result = prologThread.QueryAsyncResult();

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public bool CheckCellElement((int x, int y) coord, string element)
    {
        return bool.Parse(prologThread.Query($"is_true(situation([{coord.x}, {coord.y}], {element}))").ElementAt(0).Item1);
    }

    public List<Vector2Int> CheckElement(string element)
    {
        List<Vector2Int> listCoordElem = new();

        prologThread.QueryAsync($"list_element([Col, Row], {element})", false);

        bool more_results = true;

        while (more_results)
        {
            Vector2Int coord = new(0, 0);
            bool colRetrieved = false;

            foreach (Tuple<string, string> answer in prologThread.QueryAsyncResult())
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
                    more_results = false;
            }
        }
        return listCoordElem;
    }


    /***************** KB I/O *****************/
    public void AddToKB(string predicate, bool verifyKB)
    {
        if (!verifyKB || (verifyKB && prologThread.Query(predicate).ElementAt(0).Item1 == "false"))
            prologThread.Query($"assertz({predicate})");
    }

    public void RemoveFromKB(string predicate)
    {
        prologThread.Query($"retract({predicate})");
    }

    public void AddCellContentKB(Vector2Int coord, string cellContent)
    {
        AddToKB($"cell([{coord.x}, {coord.y}], {cellContent})", true);
    }

    public void RemoveCellContentKB(Vector2Int coord, string cellContent)
    {
        RemoveFromKB($"cell([{coord.x}, {coord.y}], {cellContent})");
    }

    public List<string> GetPersonalities(Human agent)
    {
        prologThread.QueryAsync($"personality({agent.agentName}, Personality)", false);

        List<string> listPersonalities = new();
        bool more_results = true;

        while (more_results)
        {
            foreach (Tuple<string, string> answer in prologThread.QueryAsyncResult())
            {
                if (answer.Item1 == "null" || answer.Item2 == "null")
                    more_results = false;
                else
                    listPersonalities.Add(answer.Item2);
            }
        }

        return listPersonalities;
    }



    /***************** LOGS *****************/
    public void PrintKB(List<Human> agents)
    {
        if (debugFileLog)
            ResetDebugKB();

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
            WriteInDebugKB("% ------------------");

        PrintAgentMovements(debugFileLog, consoleLog, agents);

        PrintCellContent(debugFileLog, consoleLog);

        void PrintAgentMovements(bool debugFile, bool consoleLog, List<Human> agents)
        {
            foreach (Human agent in agents)
            {
                string stackTrace = "% ";
                foreach (Vector2Int movement in agent.pastMovements)
                    stackTrace += movement.ToString();

                if (consoleLog)
                    Debug.Log(stackTrace);
                if (debugFile)
                    WriteInDebugKB(stackTrace);
            }
        }

        void PrintCellContent(bool debugFile, bool consoleLog)
        {
            prologThread.QueryAsync($"cell([Col, Row], Element)", false);

            List<string> listCol = new();
            List<string> listRow = new();
            List<string> listElement = new();

            bool more_results = true;

            while (more_results)
            {
                foreach (Tuple<string, string> answer in prologThread.QueryAsyncResult())
                {
                    if (answer.Item1 == "Col")
                        listCol.Add(answer.Item2);
                    else if (answer.Item1 == "Row")
                        listRow.Add(answer.Item2);
                    else if (answer.Item1 == "Element")
                        listElement.Add(answer.Item2);
                    else if (answer.Item1 == "null" && answer.Item2 == "null")
                        more_results = false;
                }
            }

            if (listCol.Count() == listRow.Count() && listRow.Count() == listElement.Count())
            {
                for (int i = 0; i < listCol.Count(); i++)
                {
                    if (consoleLog)
                        Debug.Log($"cell([{listCol[i]}, {listRow[i]}], {listElement[i]}).");
                    if (debugFile)
                        WriteInDebugKB($"cell([{listCol[i]}, {listRow[i]}], {listElement[i]}).");
                }
            }
        }

        void PrintGlobalVariables(string variable, bool debugFile, bool consoleLog)
        {
            prologThread.QueryAsync($"{variable}(Element, Characteristic)", false);

            List<string> listElements = new();
            List<string> listCharacteristics = new();

            bool more_results = true;

            while (more_results)
            {
                foreach (Tuple<string, string> answer in prologThread.QueryAsyncResult())
                {
                    if (answer.Item1 == "Element")
                        listElements.Add(answer.Item2);
                    else if (answer.Item1 == "Characteristic")
                        listCharacteristics.Add(answer.Item2);
                    else if (answer.Item1 == "null" && answer.Item2 == "null")
                        more_results = false;
                }
            }

            if (listElements.Count() == listCharacteristics.Count())
            {
                for (int i = 0; i < listElements.Count(); i++)
                {
                    if (consoleLog)
                        Debug.Log($"{variable}({listElements[i]}, {listCharacteristics[i]}).");
                    if (debugFile)
                        WriteInDebugKB($"{variable}({listElements[i]}, {listCharacteristics[i]}).");
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
        method.Invoke(new object(), null);
    }
#endif

    /***************** DEBUG FILE *****************/
    private void ResetDebugKB()
    {
        string oldVersion;
        string newVersion = "";
        string stoppingLine = "% DYNAMIC PART OF THE KNOWLEDGE BASE";

        StreamReader streamReader = File.OpenText(debugPrologFilePath);
        while ((oldVersion = streamReader.ReadLine()) != null)
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
        File.WriteAllText(debugPrologFilePath, newVersion);
    }

    public void WriteInDebugKB(string prologText)
    {
        if (!File.Exists(debugPrologFilePath))
        {
            // Create a file to write to.
            using StreamWriter streamWriter = File.CreateText(debugPrologFilePath);
            streamWriter.WriteLine(prologText);
        }
        else
        {
            // Append text to existing file
            using StreamWriter streamWriter = File.AppendText(debugPrologFilePath);
            streamWriter.WriteLine(prologText);
        }
    }
}
