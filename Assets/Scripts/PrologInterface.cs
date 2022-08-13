using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Assertions;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Callback;
using System.Reflection;
using System;
using System.IO;

/// <summary>
/// Interface between the Unity C# code and the Prolog Engine
/// via the SwiPlCs Plugin
/// </summary>
public class PrologInterface : MonoBehaviour
{
    [SerializeField] private bool debugFileLog = true;
    private bool consoleLog = false;
    [SerializeField] private string query = "cell(X, Y, Z)";
    [SerializeField] private string[] args = new string[] { "X", "Y", "Z" };
    [SerializeField] private bool askQuery = false;

    private string prologFilePath = Path.Combine(Application.streamingAssetsPath, "kb.pl");
    private string debugPrologFilePath = Path.Combine(Application.streamingAssetsPath, "debugkb.pl");

    /* Clean close the Prolog Engine */
    public void OnApplicationQuit()
    {
        PlEngine.PlCleanup();
    }

    void Update()
    {
        /* Prolog query inside the Unity Inspector */
        if (askQuery)
        {
            try
            {
                using (PlQuery q = new PlQuery(query))
                {
                    foreach (PlQueryVariables v in q.SolutionVariables)
                    {
                        foreach (string s in args)
                            Debug.Log(s + " = " + v[s].ToString());
                    }
                }
            }
            catch (PlException ex)
            {
                Debug.Log(ex.ToString());
            }
            finally
            {
                PlEngine.PlCleanup();
            }

            askQuery = false;
        }
    }

    public void InitialiseGameKB(int nbWumpus, Human agent)
    {
        string[] param = { "-q", "-f", prologFilePath };  // suppressing informational & banner messages

        if (!PlEngine.IsInitialized)
            PlEngine.Initialize(param);

        // AddToKB($"grid_coord({gridMin.col}, {gridMin.row}, {gridMax.col}, {gridMax.row})", true);
        // AddToKB($"nb_gold({nbGold})", true);
        AddToKB($"nb_gold({agent.agentName}, {0})", true);
        AddToKB($"nb_arrow({agent.agentName}, {nbWumpus})", true);
        // AddToKB($"nb_wumpus({nbWumpus})", true);
        // AddToKB($"nb_wumpus_dead({0})", true);
        AddToKB($"intelligence({agent.agentName}, {agent.intelligence})", true);
        AddToKB($"strength({agent.agentName}, {agent.strength})", true);
        AddToKB($"dexterity({agent.agentName}, {agent.dexterity})", true);

        // foreach (string personality in agent.getAgentPersonalities())
        // AddToKB($"personality:{personality}({agent.agentName})", false);
    }

    public String NextMove(Human agent)
    {
        using (PlQuery queryNextMove = new PlQuery("move", new PlTermV(new PlTerm[] { new PlTerm(agent.agentName), new PlTerm("Move") })))
        {
            foreach (PlTermV solution in queryNextMove.Solutions)
            {
                return (string)solution[1];
            }
        }
        return "Default";
    }

    public string RandomMove(Human agent)
    {
        using (PlQuery queryRandomMove = new PlQuery("random_move", new PlTermV(new PlTerm[] { new PlTerm(agent.agentName), new PlTerm("Move") })))
        {
            foreach (PlTermV solution in queryRandomMove.Solutions)
            {
                return (string)solution[1];
            }
        }
        return "Default";
    }

    public String NextAction(Human agent)
    {
        using (PlQuery queryNextAction = new PlQuery("action", new PlTermV(new PlTerm[] { new PlTerm(agent.agentName), new PlTerm("Action") })))
        {
            foreach (PlTermV solution in queryNextAction.Solutions)
            {
                return (string)solution[1];
            }
        }
        return "Default";
    }

    public Boolean CheckCellElement(Coordinates coords, string element)
    {
        return PlQuery.PlCall($"is_true(cell2({coords.col}, {coords.row}, {element}))");
    }

    public List<Coordinates> CheckElement(string element)
    {
        List<Coordinates> listCoordsElement = new List<Coordinates>();

        using (PlQuery checkElement = new PlQuery("list_element", new PlTermV(new PlTerm[] { new PlTerm("Col"), new PlTerm("Row"), new PlTerm(element) })))
        {
            foreach (PlTermV solution in checkElement.Solutions)
            {
                listCoordsElement.Add(new Coordinates((int)solution[0], (int)solution[1]));
            }
        }

        return listCoordsElement;
    }


    /***************** KB I/O *****************/
    public void AddToKB(string predicate, Boolean verifyKB)
    {
        // If fact not already in KB, add it
        if (verifyKB)
        {
            if (PlQuery.PlCall($"{predicate}") == false)
                PlQuery.PlCall($"assertz({predicate})");
        }
        else
            PlQuery.PlCall($"assertz({predicate})");
    }

    public void RemoveFromKB(string predicate)
    {
        PlQuery.PlCall($"retractall({predicate})");
    }

    public void AddCellContentKB(Coordinates coords, string cellContent)
    {
        // If fact not already in KB, add it
        if (PlQuery.PlCall($"cell({coords.col}, {coords.row}, {cellContent})") == false)
            PlQuery.PlCall($"assertz(cell({coords.col}, {coords.row}, {cellContent}))");
    }

    public void RemoveCellContentKB(Coordinates coords, string cellContent)
    {
        PlQuery.PlCall($"retractall(cell({coords.col}, {coords.row}, {cellContent}))");
    }


    /***************** LOGS *****************/
    public void PrintKB(Human agent)
    {
        if (debugFileLog)
            ResetDebugKB();

        // ClearLog();

        // PrintGlobalVariables("grid_coord", "Grid Coords: ", debugFileLog, consoleLog);
        // PrintGlobalVariables("nb_wumpus", "Initial number of Wumpus: ", debugFileLog, consoleLog);
        // PrintGlobalVariables("nb_wumpus_dead", "Wumpus killed: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_arrow", "Initial number of arrows: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_gold", "Initial number of gold: ", debugFileLog, consoleLog);
        // PrintGlobalVariables("nb_gold_agent", "Number of gold picked up: ", debugFileLog, consoleLog);
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
            // Print Cells
            using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm("Col"), new PlTerm("Row"), new PlTerm("Element"))))
            {
                foreach (PlTermV solution in queryCell.Solutions)
                {
                    if (consoleLog)
                        Debug.Log("Cell= Col: " + solution[0].ToString() + ", Row: " + solution[1].ToString() + " = " + solution[2].ToString() + "\n");
                    if (debugFile)
                        WriteInDebugKB("cell(" + solution[0] + "," + solution[1] + "," + solution[2] + ").");
                }
            }
        }

        void PrintGlobalVariables(string variable, string message, Boolean debugFile, Boolean consoleLog)
        {
            // if (variable == "grid_coord")
            // {
            //     using (PlQuery queryVariable = new PlQuery(variable, new PlTermV(new PlTerm[] { new PlTerm("MinCol"), new PlTerm("MinRow"), new PlTerm("MaxCol"), new PlTerm("MaxRow") })))
            //     {
            //         foreach (PlTermV solution in queryVariable.Solutions)
            //         {
            //             if (debugFile)
            //                 WriteInDebugKB(variable + "(" + solution[0] + "," + solution[1] + "," + solution[2] + "," + solution[3] + ").");
            //         }
            //     }
            // }
            // else if (variable == "personality")
            // {

            //     using (PlQuery queryVariable = new PlQuery(variable, new PlTermV(new PlTerm[] { new PlTerm("Element"), new PlTerm("Personality") })))
            //     {
            //         foreach (PlTermV solution in queryVariable.Solutions)
            //         {
            //             if (debugFile)
            //                 WriteInDebugKB(variable + "(" + solution[0] + "," + solution[1] + ").");
            //         }
            //     }
            // }
            // if (variable == "personality" || variable == "intelligence" || variable == "strength" || variable == "dexterity")
            // {

            using (PlQuery queryVariable = new PlQuery(variable, new PlTermV(new PlTerm[] { new PlTerm("Element"), new PlTerm("Characteristic") })))
            {
                foreach (PlTermV solution in queryVariable.Solutions)
                {
                    if (debugFile)
                        WriteInDebugKB(variable + "(" + solution[0] + "," + solution[1] + ").");
                }
            }
            // }
            // else
            // {
            //     using (PlQuery queryVariable = new PlQuery(variable, new PlTermV(new PlTerm("Element"))))
            //     {
            //         foreach (PlTermV solution in queryVariable.Solutions)
            //         {
            //             if (consoleLog)
            //                 Debug.Log(message + solution[0].ToString() + "\n");

            //             if (debugFile)
            //                 WriteInDebugKB(variable + "(" + solution[0] + ").");
            //         }
            //     }
            // }
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
