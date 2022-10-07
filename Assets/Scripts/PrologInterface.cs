using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Assertions;
using System.Reflection;
using System.IO;
using System;
using System.Linq;

public class PrologInterface : MonoBehaviour
{
    [SerializeField] private bool debugFileLog = true;
    private bool consoleLog = false;
    [SerializeField] private string query = "situation([X, Y], Z)";
    [SerializeField] private bool askQuery = false;

    private string prologFilePath = Path.Combine(Application.streamingAssetsPath, "kb.pl");
    private string debugPrologFilePath = Path.Combine(Application.streamingAssetsPath, "debugkb.pl");

    private PrologMQI mqi;
    private PrologMQI mqi2;
    PrologThread prologThread;
    PrologThread prologThread2;

    void Update()
    {
        /* Prolog query inside the Unity Inspector */
        if (askQuery)
        {
            prologThread.query_async(query, false);

            Debug.Log("Query: " + query);

            bool more_results = true;

            while (more_results)
            {
                foreach (Tuple<string, string> answer in prologThread.query_async_result())
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

    public void InitialiseGameKB(Human agent, Human agent2, int nbWumpus)
    {
        this.mqi = new PrologMQI();
        this.mqi2 = new PrologMQI();
        this.prologThread = mqi.create_thread();
        this.prologThread2 = mqi2.create_thread();

        this.prologThread.query("consult('" + prologFilePath + "')");
        this.prologThread2.query("consult('" + prologFilePath + "')");

        AddToKB(agent, $"nb_gold({agent.name}, {0})", true);
        AddToKB(agent, $"nb_arrow({agent.name}, {nbWumpus})", true);
        AddToKB(agent, $"intelligence({agent.name}, {agent.intelligence})", true);
        AddToKB(agent, $"strength({agent.name}, {agent.strength})", true);
        AddToKB(agent, $"dexterity({agent.name}, {agent.dexterity})", true);

        AddToKB(agent2, $"nb_gold({agent2.name}, {0})", true);
        AddToKB(agent2, $"nb_arrow({agent2.name}, {nbWumpus})", true);
        AddToKB(agent2, $"intelligence({agent2.name}, {agent2.intelligence})", true);
        AddToKB(agent2, $"strength({agent2.name}, {agent2.strength})", true);
        AddToKB(agent2, $"dexterity({agent2.name}, {agent2.dexterity})", true);
    }

    public String NextMove(Human agent)
    {
        prologThread.query_async($"move({agent.name}, Move)", false);
        List<Tuple<string, string>> result = prologThread.query_async_result();

        if (agent.id != 1)
        {
            prologThread2.query_async($"move({agent.name}, Move)", false);
            result = prologThread2.query_async_result();
        }

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public string RandomMove(Human agent)
    {
        prologThread.query_async($"random_move({agent.name}, Move)", false);
        List<Tuple<string, string>> result = prologThread.query_async_result();

        if (agent.id != 1)
        {
            prologThread2.query_async($"random_move({agent.name}, Move)", false);
            result = prologThread2.query_async_result();
        }

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public String NextAction(Human agent)
    {
        prologThread.query_async($"action({agent.name}, Action)", false);
        List<Tuple<string, string>> result = prologThread.query_async_result();

        if (agent.id != 1)
        {
            prologThread2.query_async($"action({agent.name}, Action)", false);
            result = prologThread2.query_async_result();
        }

        if (result.Count == 0)
            return "Default";
        else
            return result.First().Item2;
    }

    public Boolean CheckCellElement(Coordinates coords, string element)
    {
        return bool.Parse(prologThread.query($"is_true(situation([{coords.col}, {coords.row}], {element}))").ElementAt(0).Item1);
    }

    public List<Coordinates> CheckElement(string element, Human agent)
    {
        List<Coordinates> listCoordsElement = new List<Coordinates>();

        if (agent.id == 1)
            prologThread.query_async($"list_element([Col, Row], {element})", false);
        else
            prologThread2.query_async($"list_element([Col, Row], {element})", false);

        bool more_results = true;

        while (more_results)
        {
            Coordinates coords = new Coordinates(0, 0);
            bool colRetrieved = false;


            List<Tuple<string, string>> answers = new List<Tuple<string, string>>();

            if (agent.id == 1)
                answers = prologThread.query_async_result();
            else
                answers = prologThread2.query_async_result();


            foreach (Tuple<string, string> answer in answers)
            {
                if (answer.Item1 == "Col")
                {
                    coords.col = int.Parse(answer.Item2);
                    colRetrieved = true;
                }
                else if (answer.Item1 == "Row")
                {
                    coords.row = int.Parse(answer.Item2);
                    if (colRetrieved && coords.col != null && coords.row != null)
                        listCoordsElement.Add(coords);
                }
                else if (answer.Item1 == "null" && answer.Item2 == "null")
                    more_results = false;
            }
        }

        return listCoordsElement;
    }


    /***************** KB I/O *****************/
    public void AddToKB(Human agent, string predicate, Boolean verifyKB)
    {
        if (agent.id == 1)
        {
            if (!verifyKB || (verifyKB && this.prologThread.query(predicate).ElementAt(0).Item1 == "false"))
                this.prologThread.query($"assertz({predicate})");
        }
        else
        {
            if (!verifyKB || (verifyKB && this.prologThread2.query(predicate).ElementAt(0).Item1 == "false"))
                this.prologThread2.query($"assertz({predicate})");
        }
    }

    public void RemoveFromKB(Human agent, string predicate)
    {
        if (agent.id == 1)
            this.prologThread.query($"retract({predicate})");
        else
            this.prologThread2.query($"retract({predicate})");
    }

    public void AddCellContentKB(Human agent, Coordinates coords, string cellContent)
    {
        AddToKB(agent, $"cell([{coords.col}, {coords.row}], {cellContent})", true);
    }

    public void RemoveCellContentKB(Human agent, Coordinates coords, string cellContent)
    {
        RemoveFromKB(agent, $"cell([{coords.col}, {coords.row}], {cellContent})");
    }


    /***************** LOGS *****************/
    public void PrintKB(Human agent)
    {
        if (debugFileLog)
            ResetDebugKB();

        // ClearLog();

        PrintGlobalVariables("nb_arrow", "Initial number of arrows: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_gold", "Initial number of gold: ", debugFileLog, consoleLog);
        PrintGlobalVariables("intelligence", "Intelligence: ", debugFileLog, consoleLog);
        PrintGlobalVariables("strength", "Strength: ", debugFileLog, consoleLog);
        PrintGlobalVariables("dexterity", "Dexterity: ", debugFileLog, consoleLog);
        PrintGlobalVariables("personality", "Personality: ", debugFileLog, consoleLog);

        if (consoleLog)
            Debug.Log("------------------\n");
        if (debugFileLog)
            WriteInDebugKB("% ------------------");

        PrintAgentMovements(debugFileLog, consoleLog);

        PrintCellContent(debugFileLog, consoleLog);

        void PrintAgentMovements(Boolean debugFile, Boolean consoleLog)
        {
            string stackTrace = "% ";
            foreach (Coordinates movement in agent.pastMovements)
            {
                stackTrace += movement.ToString();
            }

            if (consoleLog)
                Debug.Log(stackTrace);
            if (debugFile)
                WriteInDebugKB(stackTrace);
        }

        void PrintCellContent(Boolean debugFile, Boolean consoleLog)
        {
            prologThread.query_async($"cell([Col, Row], Element)", false);

            List<String> listCol = new List<String>();
            List<String> listRow = new List<String>();
            List<String> listElement = new List<String>();

            bool more_results = true;

            while (more_results)
            {
                foreach (Tuple<string, string> answer in prologThread.query_async_result())
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

        void PrintGlobalVariables(string variable, string message, Boolean debugFile, Boolean consoleLog)
        {
            prologThread.query_async($"{variable}(Element, Characteristic)", false);

            List<String> listElements = new List<String>();
            List<String> listCharacteristics = new List<String>();

            bool more_results = true;

            while (more_results)
            {
                foreach (Tuple<string, string> answer in prologThread.query_async_result())
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
            {
                newVersion += oldVersion + Environment.NewLine;
            }
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
            using (StreamWriter streamWriter = File.CreateText(debugPrologFilePath)) // Create a file to write to.
            {
                streamWriter.WriteLine(prologText);
            }
        }

        using (StreamWriter streamWriter = File.AppendText(debugPrologFilePath)) // Add text to file
        {
            streamWriter.WriteLine(prologText);
        }
    }
}
