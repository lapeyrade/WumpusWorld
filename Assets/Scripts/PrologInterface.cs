using System.Collections.Generic;
using UnityEngine;
using SbsSW.SwiPlCs;
using System.Reflection;
using System;
using System.IO;

public class PrologInterface : MonoBehaviour
{
    [SerializeField]
    private bool debugEnabled = true;

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
        // using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm(coords.col), new PlTerm(coords.row), new PlTerm(element))))
        // {
        //     foreach (PlTermV solution in queryCell.Solutions)
        //     {
        //         return (string)solution[0];
        //     }
        // }
        // return "undefined";
        // Debug.Log(PlQuery.PlCall($"cell({coords.col}, {coords.row}, {element})"));
        Debug.Log(coords.col + " " + coords.row + " " + PlQuery.PlCall($"cell({coords.col}, {coords.row}, wumpus)"));
        Debug.Log(PlQuery.PlCall($"cell({coords.col}, {coords.row}, wumpusyes)"));
        // Debug.Log(PlQuery.PlCall($"cell({coords.col}, {coords.row}, agent)"));
        return false;
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

    public Coordinates GetAgentPosition()
    {
        using (PlQuery queryAgentPosition = new PlQuery("cell", new PlTermV(new PlTerm[] { new PlTerm("Col"), new PlTerm("Row"), new PlTerm("agent") })))
        {
            foreach (PlTermV solution in queryAgentPosition.Solutions)
            {
                return new Coordinates((int)solution[0], (int)solution[1]);
            }
        }
        return new Coordinates(-1, -1);
    }

    public void PopPrologMovementStack()
    {
        PlQuery.PlCall("pop_stack");
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

    public void PrintKB()
    {
        if (debugEnabled)
            ResetDebugKB();

        // ClearLog();
        PrintGlobalVariables("nb_wumpus", "Initial number of Wumpus: ");
        PrintGlobalVariables("nb_wumpus_dead", "Wumpus killed: ");
        PrintGlobalVariables("nb_arrow", "Initial number of arrows: ");
        PrintGlobalVariables("nb_arrow_used", "Arrows shot: ");
        PrintGlobalVariables("nb_gold", "Initial number of gold: ");
        PrintGlobalVariables("nb_gold_agent", "Number of gold picked up: ");
        // PrintSingleElement("all_golds_found", "Is all gold found? ");


        // Print agent previous coordinates
        // using (PlQuery queryPreviousCoordsCol = new PlQuery("stack_moves", new PlTermV(new PlTerm[] { new PlTerm("PrevCol"), new PlTerm("PrevRows") })))
        // {
        //     foreach (PlTermV solution in queryPreviousCoordsCol.Solutions)
        //     {
        //         Debug.Log("Previous Coords Col: " + solution[0].ToString() + "\n");
        //         Debug.Log("Previous Coords Row: " + solution[1].ToString() + "\n");
        //         if (debugEnabled)
        //             WriteInDebugKB("stack_moves(" + solution[0] + " | " + solution[1] + ").");
        //     }
        // }

        Debug.Log("------------------\n");
        if (debugEnabled)
        {
            WriteInDebugKB("% ------------------");
        }


        // Print Cells
        using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm("Col"), new PlTerm("Row"), new PlTerm("Element"))))
        {
            foreach (PlTermV solution in queryCell.Solutions)
            {
                Debug.Log("Cell= Col: " + solution[0].ToString() + ", Row: " + solution[1].ToString() + " = " + solution[2].ToString() + "\n");
                if (debugEnabled)
                    WriteInDebugKB("cell(" + solution[0] + "," + solution[1] + "," + solution[2] + ").");
            }
        }

        void PrintGlobalVariables(string variable, string message)
        {
            using (PlQuery queryVariable = new PlQuery(variable, new PlTermV(new PlTerm("Element"))))
            {
                foreach (PlTermV solution in queryVariable.Solutions)
                {
                    Debug.Log(message + solution[0].ToString() + "\n");
                    if (debugEnabled)
                        WriteInDebugKB(variable + "(" + solution[0] + ").");
                }
            }
        }

        void PrintSingleElement(string element, string message)
        {
            using (PlQuery queryElement = new PlQuery(element, new PlTermV(new PlTerm("Element"))))
            {
                foreach (PlTermV solution in queryElement.Solutions)
                {
                    Debug.Log(message + solution[0].ToString() + "\n");
                    if (debugEnabled)
                        WriteInDebugKB(element + "(" + solution[0] + ").");
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

    /***************** DEBUG *****************/
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
