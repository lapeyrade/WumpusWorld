using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Exceptions;
using System.Reflection;
using System;
using System.IO;

public class Action
{
    private string prologFilePath = @"./Assets/Scripts/knowledgeBase.pl";
    private string debugPrologFilePath = @"./Assets/Scripts/debugKB.pl";
    private bool debugEnabled;

    public Action(int wumpuses, int golds, Coordinates coords, bool debug)
    {
        debugEnabled = debug;
        InitialiseProlog();
        InitialiseGameKB(wumpuses, golds, coords);
        printKB();
    }

    public Coordinates NextAction(string action, Coordinates coords)
    {
        if(action == "prolog")
            return NextMoveProlog(coords);
        else 
            return RandomMoveProlog(coords);
    }

    private void InitialiseProlog()
    {
        // suppressing informational and banner messages
        string[] param = {"-q", "-f", prologFilePath};  
        PlEngine.Initialize(param);
        PlQuery.PlCall("resetKB");
    }

    private void InitialiseGameKB(int wumpuses, int golds, Coordinates coords)
    {
        PlQuery.PlCall($"initGameAttributes({wumpuses}, {golds})");
        PlQuery.PlCall($"initAgent({coords.x}, {coords.y})");
    }
    
    private Coordinates NextMoveProlog(Coordinates coords)
    {
        using(PlQuery query = new PlQuery("nextMove", new PlTermV(new PlTerm[] {new PlTerm(coords.x), new PlTerm(coords.y), new PlTerm("NewX"), new PlTerm("NewY")})))
        {
            foreach (PlTermV solution in query.Solutions)
            {
                return new Coordinates((int)solution[2], (int)solution[3]);
            }
        }
        Debug.Log("Random Move.");
        return RandomMoveProlog(coords);
    }

    private Coordinates RandomMoveProlog(Coordinates coords)
    {
        using(PlQuery query = new PlQuery("randomMove", new PlTermV(new PlTerm[] {new PlTerm(coords.x), new PlTerm(coords.y), new PlTerm("NewX"), new PlTerm("NewY")})))
        {
            foreach (PlTermV solution in query.Solutions)
            {
                return new Coordinates((int)solution[2], (int)solution[3]);
            }
        }
        Debug.Log("Random Move failed. Going Right.");
        return new Coordinates(coords.x+1, coords.y);
    }

    public void CheckNearCells(Coordinates coords)
    {
        PlQuery.PlCall($"checkNearCells({coords.x}, {coords.y})");
    }

    public Coordinates WumpusFound()
    {
        using(PlQuery query = new PlQuery("cell", new PlTermV(new PlTerm[] {new PlTerm("X"), new PlTerm("Y"), new PlTerm("wumpus")})))
        {
            foreach (PlTermV solution in query.Solutions)
            {
                return(new Coordinates((int)solution[0], (int)solution[1]));
            }
        }
        return null;
    }

    public List<string> CheckCell(Coordinates coords)
    {
        List<string> cellContent = new List<string>();
        using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm(coords.x), new PlTerm(coords.y), new PlTerm("State"))))
        {
            foreach (PlTermV solution in queryCell.Solutions)
            {
                cellContent.Add((string)solution[2]);
            }
        }
        return cellContent;
    }

    public void BumpIntoWall()
    {
        PlQuery.PlCall($"popStack(stackX)");
        PlQuery.PlCall($"popStack(stackY)");
    }

    public void printKB()
    {
        if(debugEnabled)
            resetDebugKB();

        ClearLog();
        printSingleElement("wumpusTotal", "Initial number of Wumpus: ");
        printSingleElement("wumpusKilled", "Wumpus killed: ");
        printSingleElement("arrowTotal", "Initial number of arrows: ");
        printSingleElement("arrowUsed", "Arrows shot: ");
        printSingleElement("goldTotal", "Initial number of gold: ");
        printSingleElement("goldAgent", "Number of gold picked up: ");


        // Print agent previous coordinates
        using (PlQuery queryPreviousCoordsX = new PlQuery("nb_getval", new PlTermV(new PlTerm("stackX"), new PlTerm("PrevX"))))
        { 
            foreach (PlTermV solution in queryPreviousCoordsX.Solutions)
            {
                Debug.Log("Previous Coords X: " + solution[1].ToString());
                if(debugEnabled)
                    WriteInDebugKB("stackX(" + solution[1]+ ").");
            }
        }
        using (PlQuery queryPreviousCoordsY = new PlQuery("nb_getval", new PlTermV(new PlTerm("stackY"), new PlTerm("PrevY"))))
        { 
            foreach (PlTermV solution in queryPreviousCoordsY.Solutions)
            {
                Debug.Log("Previous Coords Y: " + solution[1].ToString());
                if(debugEnabled)
                    WriteInDebugKB("stackY(" + solution[1]+ ").");
            }
        }

        // Print Cells
        using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm("X"), new PlTerm("Y"), new PlTerm("State"))))
        { 
            foreach (PlTermV solution in queryCell.Solutions)
            {
                Debug.Log("Cell X: " + solution[0].ToString() +  ", Y: " + solution[1].ToString() + " = " + solution[2].ToString());
                if(debugEnabled)
                    WriteInDebugKB("cell(" + solution[0] + "," + solution[1] + "," + solution[2] + ").");
            }
        }

        void printSingleElement(string element, string message)
        {
            using (PlQuery queryElement = new PlQuery(element, new PlTermV(new PlTerm("X"))))
            {
                foreach (PlTermV solution in queryElement.Solutions)
                {
                    Debug.Log(message + solution[0].ToString());
                    if(debugEnabled)
                        WriteInDebugKB(element + "(" + solution[0] + ").");
                }
            }
        }
    }

    public void ClearLog()
    {
        var assembly = Assembly.GetAssembly(typeof(UnityEditor.Editor));
        var type = assembly.GetType("UnityEditor.LogEntries");
        var method = type.GetMethod("Clear");
        method.Invoke(new object(), null);
    }

    public void AddFactKB(string prologFact)
    {
        // If fact not already in KB, add it
        if (PlQuery.PlCall($"{prologFact}") == false)
        {
            PlQuery.PlCall($"assertz({prologFact})");
        }
    }

    public void RemoveFromKB(string prologFact)
    {
        PlQuery.PlCall($"retractall({prologFact})");
    }

    /***************** DEBUG *****************/
    private void resetDebugKB()
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

        string oldVersion;
        bool alreadyPresent = false;

        StreamReader streamReader = File.OpenText(debugPrologFilePath);
        while ((oldVersion = streamReader.ReadLine()) != null)
        {
            if (oldVersion.Contains(prologText))
            {
                alreadyPresent = true;
            }
        }
        streamReader.Close();
        
        if (!alreadyPresent)
        {
            using (StreamWriter sw = File.AppendText(debugPrologFilePath)) // Add text to file
                {
                    sw.WriteLine(prologText);
                }
        }

    }
}
