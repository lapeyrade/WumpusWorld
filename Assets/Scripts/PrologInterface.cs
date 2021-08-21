using System.Collections.Generic;
using UnityEngine;
using SbsSW.SwiPlCs;
using System.Reflection;
using System;
using System.IO;

/// <summary>
/// Interface between the Unity C# code and the Prolog Engine
/// via the SwiPlCs Plugin
/// </summary>
public class PrologInterface : MonoBehaviour
{
    [SerializeField]
    private bool debugFileLog = true;
    private bool consoleLog = false;

    private string prologFilePath = Path.Combine(Application.streamingAssetsPath, "KB.pl");
    private string debugPrologFilePath = Path.Combine(Application.streamingAssetsPath, "debugKB.pl");

    public void InitialiseGameKB(Coordinates startCoords, int nbGold, int nbWumpus)
    {
        string[] param = { "-q", "-f", prologFilePath };  // suppressing informational & banner messages

        PlEngine.Initialize(param);
        ResetKB();
        AddToKB($"nb_gold({nbGold})");
        AddToKB($"nb_gold_agent({0})");
        AddToKB($"nb_arrow({nbWumpus})");
        AddToKB($"nb_arrow_used({0})");
        AddToKB($"nb_wumpus({nbWumpus})");
        AddToKB($"nb_wumpus_dead({0})");
    }
    public void ResetKB()
    {
        RemoveFromKB("cell(_, _, _)");
        RemoveFromKB("nb_wumpus(_)");
        RemoveFromKB("nb_wumpus_dead(_)");
        RemoveFromKB("nb_arrow_used(_)");
        RemoveFromKB("nb_arrow(_)");
        RemoveFromKB("nb_gold(_)");
        RemoveFromKB("nb_gold_agent(_)");
        RemoveFromKB("wumpus_checked(_)");
    }

    public String NextMove()
    {
        using (PlQuery queryNextMove = new PlQuery("next_move", new PlTermV(new PlTerm[] { new PlTerm("Move") })))
        {
            foreach (PlTermV solution in queryNextMove.Solutions)
            {
                return (string)solution[0];
            }
        }
        return "Default";
    }

    public string RandomMove()
    {
        using (PlQuery queryRandomMove = new PlQuery("random_move", new PlTermV(new PlTerm[] { new PlTerm("Move") })))
        {
            foreach (PlTermV solution in queryRandomMove.Solutions)
            {
                return (string)solution[0];
            }
        }
        return "Default";
    }

    public String NextAction()
    {
        using (PlQuery queryNextAction = new PlQuery("next_action", new PlTermV(new PlTerm[] { new PlTerm("Action") })))
        {
            foreach (PlTermV solution in queryNextAction.Solutions)
            {
                return (string)solution[0];
            }
        }
        return "Default";
    }

    public List<Coordinates> ElementCoordinates(string element)
    {
        List<Coordinates> elementCoordinates = new List<Coordinates>();
        using (PlQuery queryWorld = new PlQuery("cell", new PlTermV(new PlTerm[] { new PlTerm("Col"), new PlTerm("Row"), new PlTerm(element) })))
        {
            foreach (PlTermV solution in queryWorld.Solutions)
            {
                elementCoordinates.Add(new Coordinates((int)solution[0], (int)solution[1]));
            }
        }
        return elementCoordinates;
    }

    public Boolean CheckCellElement(Coordinates coords, string element)
    {
        return PlQuery.PlCall($"cell2({coords.col}, {coords.row}, {element})");
    }

    public Boolean CheckCellElementFalse(Coordinates coords, string element)
    {
        return PlQuery.PlCall($"tnot(cell2({coords.col}, {coords.row}, {element}))");
    }

    public List<string> CheckCell(Coordinates coords)
    {
        List<string> cellContent = new List<string>();
        using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm(coords.col), new PlTerm(coords.row), new PlTerm("Element"))))
        {
            foreach (PlTermV solution in queryCell.Solutions)
            {
                cellContent.Add((string)solution[2]);
            }
        }
        return cellContent;
    }

    /***************** KB I/O *****************/
    public void AddToKB(string predicate)
    {
        // If fact not already in KB, add it
        if (PlQuery.PlCall($"{predicate}") == false)
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
        {
            PlQuery.PlCall($"assertz(cell({coords.col}, {coords.row}, {cellContent}))");
        }
    }

    public void RemoveCellContentKB(Coordinates coords, string cellContent)
    {
        PlQuery.PlCall($"retractall(cell({coords.col}, {coords.row}, {cellContent}))");
    }

    /***************** LOGS *****************/
    public void PrintKB(Agent agent)
    {
        if (debugFileLog)
            ResetDebugKB();

        // ClearLog();

        PrintGlobalVariables("nb_wumpus", "Initial number of Wumpus: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_wumpus_dead", "Wumpus killed: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_arrow", "Initial number of arrows: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_arrow_used", "Arrows shot: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_gold", "Initial number of gold: ", debugFileLog, consoleLog);
        PrintGlobalVariables("nb_gold_agent", "Number of gold picked up: ", debugFileLog, consoleLog);

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
            using (PlQuery queryVariable = new PlQuery(variable, new PlTermV(new PlTerm("Element"))))
            {
                foreach (PlTermV solution in queryVariable.Solutions)
                {
                    if (consoleLog)
                        Debug.Log(message + solution[0].ToString() + "\n");
                    if (debugFile)
                        WriteInDebugKB(variable + "(" + solution[0] + ").");
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
            using (StreamWriter sw = File.CreateText(debugPrologFilePath)) // Create a file to write to.
            {
                sw.WriteLine(prologText);
            }
        }

        using (StreamWriter sw = File.AppendText(debugPrologFilePath)) // Add text to file
        {
            sw.WriteLine(prologText);
        }
    }
}
