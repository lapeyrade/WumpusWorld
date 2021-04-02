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
        string[] param = {"-q", "-f", prologFilePath};  // suppressing informational & banner messages
        PlEngine.Initialize(param);
        PlQuery.PlCall("resetKB");
    }

    public void InitialiseGameKB(int seedRandom, Coordinates startCoords, Coordinates gridMin, Coordinates gridMax, int nbGold, int nbWumpus, int nbPit)
    {
        using(PlQuery queryInitGame = new PlQuery("initGame", new PlTermV(new PlTerm[] {new PlTerm(seedRandom), new PlTerm(startCoords.col), new PlTerm(startCoords.row), new PlTerm(gridMin.col), new PlTerm(gridMin.row), new PlTerm(gridMax.col-1), new PlTerm(gridMax.row-1), new PlTerm(nbGold), new PlTerm(nbWumpus), new PlTerm(nbPit)})))
        // using(PlQuery queryInitGame = new PlQuery("initGame", new PlTermV(new PlTerm[] {new PlTerm(5), new PlTerm(1), new PlTerm(1), new PlTerm(0), new PlTerm(0), new PlTerm(6), new PlTerm(6), new PlTerm(1), new PlTerm(1), new PlTerm(3)})))
        {
            foreach (PlTermV solution in queryInitGame.Solutions)
            {
                Debug.Log("test");
            }
        }
        printKB();
    }
    
    private Coordinates NextMoveProlog(Coordinates coords)
    {
        using(PlQuery queryNextMove = new PlQuery("nextMove", new PlTermV(new PlTerm[] {new PlTerm(coords.col), new PlTerm(coords.row), new PlTerm("NewCol"), new PlTerm("NewRow")})))
        {
            foreach (PlTermV solution in queryNextMove.Solutions)
            {
                return new Coordinates((int)solution[2], (int)solution[3]);
            }
        }
        Debug.Log("Random Move.");
        return RandomMoveProlog(coords);
    }

    private Coordinates RandomMoveProlog(Coordinates coords)
    {
        using(PlQuery query = new PlQuery("randomMove", new PlTermV(new PlTerm[] {new PlTerm(coords.col), new PlTerm(coords.row), new PlTerm("NewCol"), new PlTerm("NewRow")})))
        {
            foreach (PlTermV solution in query.Solutions)
            {
                return new Coordinates((int)solution[2], (int)solution[3]);
            }
        }
        Debug.Log("Random Move failed. Going Right.");
        return new Coordinates(coords.col+1, coords.row);
    }

    public void CheckNearCells(Coordinates coords)
    {
        PlQuery.PlCall($"checkNearCells({coords.col}, {coords.row})");
    }

    public Coordinates WumpusFound()
    {
        using (PlQuery query = new PlQuery("cell", new PlTermV(new PlTerm[] {new PlTerm("Col"), new PlTerm("Row"), new PlTerm("wumpus")})))
        {
            foreach (PlTermV solution in query.Solutions)
            {
                return(new Coordinates((int)solution[0], (int)solution[1]));
            }
        }
        return null;
    }

    public List<Coordinates> CoordinatesState(string state)
    {
        List<Coordinates> stateCoordinates = new List<Coordinates>();
        using (PlQuery queryWorld = new PlQuery("world", new PlTermV(new PlTerm[] {new PlTerm("Col"), new PlTerm("Row"), new PlTerm(state)})))
        {
            foreach (PlTermV solution in queryWorld.Solutions)
            {
                stateCoordinates.Add(new Coordinates((int)solution[0], (int)solution[1]));
            }
        }
        return stateCoordinates;
    }

    public List<string> CheckCell(Coordinates coords)
    {
        List<string> cellContent = new List<string>();
        using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm(coords.col), new PlTerm(coords.row), new PlTerm("State"))))
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
        PlQuery.PlCall($"popStack(stackCol)");
        PlQuery.PlCall($"popStack(stackRow)");
    }

    public void printKB()
    {
        if(debugEnabled)
            resetDebugKB();

        ClearLog();
        printSingleElement("nbWumpus", "Initial number of Wumpus: ");
        printSingleElement("nbWumpusDead", "Wumpus killed: ");
        printSingleElement("nbArrow", "Initial number of arrows: ");
        printSingleElement("nbArrowUsed", "Arrows shot: ");
        printSingleElement("nbGold", "Initial number of gold: ");
        printSingleElement("nbGoldAgent", "Number of gold picked up: ");


        // Print agent previous coordinates
        using (PlQuery queryPreviousCoordsCol = new PlQuery("nb_getval", new PlTermV(new PlTerm("stackCol"), new PlTerm("PrevCol"))))
        { 
            foreach (PlTermV solution in queryPreviousCoordsCol.Solutions)
            {
                Debug.Log("Previous Coords Col: " + solution[1].ToString() + "\n");
                if(debugEnabled)
                    WriteInDebugKB("stackCol(" + solution[1]+ ").");
            }
        }
        using (PlQuery queryPreviousCoordsRow = new PlQuery("nb_getval", new PlTermV(new PlTerm("stackRow"), new PlTerm("PrevRow"))))
        { 
            foreach (PlTermV solution in queryPreviousCoordsRow.Solutions)
            {
                Debug.Log("Previous Coords Row: " + solution[1].ToString() + "\n");
                if(debugEnabled)
                    WriteInDebugKB("stackRow(" + solution[1]+ ").");
            }
        }

        Debug.Log("------------------\n");
        if(debugEnabled)
        {
            WriteInDebugKB("% ------------------");
        }
        
        // Print World
        using (PlQuery queryWorld = new PlQuery("world", new PlTermV(new PlTerm("Col"), new PlTerm("Row"), new PlTerm("State"))))
        { 
            foreach (PlTermV solution in queryWorld.Solutions)
            {
                Debug.Log("World= Col: " + solution[0].ToString() +  ", Row: " + solution[1].ToString() + " = " + solution[2].ToString() + "\n");
                if(debugEnabled)
                    WriteInDebugKB("world(" + solution[0] + "," + solution[1] + "," + solution[2] + ").");
            }
        }

        Debug.Log("------------------\n");
        if(debugEnabled)
        {
            WriteInDebugKB("% ------------------");
        }

        // Print Cells
        using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm("Col"), new PlTerm("Row"), new PlTerm("State"))))
        { 
            foreach (PlTermV solution in queryCell.Solutions)
            {
                Debug.Log("Cell= Col: " + solution[0].ToString() +  ", Row: " + solution[1].ToString() + " = " + solution[2].ToString() + "\n");
                if(debugEnabled)
                    WriteInDebugKB("cell(" + solution[0] + "," + solution[1] + "," + solution[2] + ").");
            }
        }


        void printSingleElement(string element, string message)
        {
            using (PlQuery queryElement = new PlQuery(element, new PlTermV(new PlTerm("Col"))))
            {
                foreach (PlTermV solution in queryElement.Solutions)
                {
                    Debug.Log(message + solution[0].ToString() + "\n");
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

        // string oldVersion;
        // bool alreadyPresent = false;

        // StreamReader streamReader = File.OpenText(debugPrologFilePath);
        // while ((oldVersion = streamReader.ReadLine()) != null)
        // {
        //     if (oldVersion.Contains(prologText))
        //     {
        //         alreadyPresent = true;
        //     }
        // }
        // streamReader.Close();
        
        // if (!alreadyPresent)
        // {
            using (StreamWriter sw = File.AppendText(debugPrologFilePath)) // Add text to file
                {
                    sw.WriteLine(prologText);
                }
        // }

    }
}
