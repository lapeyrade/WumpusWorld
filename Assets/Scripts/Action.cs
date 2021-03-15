using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Exceptions;
using System.Reflection;

public class Action
{
    private string prologFilePath = @"./Assets/Scripts/knowledgeBase.pl";

    public Action(int wumpuses, int golds, Coordinates coords)
    {
        InitialiseProlog();
        initialiseGameKB(wumpuses, golds, coords);
        printKB();  
    }

    public Coordinates NextAction(string action, Coordinates coords, Coordinates prevCoords)
    {
        Coordinates nextCoords;

        if(action == "prolog")
            nextCoords = NextMoveProlog(coords, prevCoords);
        else 
            nextCoords = RandomMoveProlog(coords);

        return nextCoords;
    }

    private void InitialiseProlog()
    {
        // suppressing informational and banner messages
        string[] param = {"-q", "-f", prologFilePath};  
        PlEngine.Initialize(param);
        PlQuery.PlCall("resetKB");
    }

    public void CheckNearCells(Coordinates coords)
    {
        PlQuery.PlCall($"checkNearCells({coords.x}, {coords.y})");
    }
    

    private Coordinates RandomMoveProlog(Coordinates coords)
    {
        using(PlQuery query = new PlQuery("randomMove", new PlTermV(new PlTerm[] {new PlTerm(coords.x), new PlTerm(coords.y), new PlTerm("PosX2"), new PlTerm("PosY2")})))
        {
            foreach (PlTermV solution in query.Solutions)
            {
                return new Coordinates((int)solution[2], (int)solution[3]) ;
            }
        }
        Debug.Log("Random Move.");
        return new Coordinates(coords.x+1, coords.y);
    }

    private Coordinates NextMoveProlog(Coordinates coords, Coordinates prevCoords)
    {
        using(PlQuery query = new PlQuery("nextMove", new PlTermV(new PlTerm[] {new PlTerm(coords.x), new PlTerm(coords.y), new PlTerm("PosX2"), new PlTerm("PosY2"), new PlTerm(prevCoords.x), new PlTerm(prevCoords.y)})))
        {
            foreach (PlTermV solution in query.Solutions)
            {
                return new Coordinates((int)solution[2], (int)solution[3]);
            }
        }
        Debug.Log("Random Move.");
        return RandomMoveProlog(coords);
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

    public void printKB()
    {
        ClearLog();
        printSingleElement("wumpusTotal", "Initial number of Wumpus: ");
        printSingleElement("wumpusKilled", "Wumpus killed: ");
        printSingleElement("arrowTotal", "Initial number of arrows: ");
        printSingleElement("arrowUsed", "Arrows shot: ");
        printSingleElement("goldTotal", "Initial number of gold: ");
        printSingleElement("goldAgent", "Number of gold picked up: ");

        // Print Cells
        using (PlQuery queryCell = new PlQuery("cell", new PlTermV(new PlTerm("X"), new PlTerm("Y"), new PlTerm("State"))))
        { 
            foreach (PlTermV solution in queryCell.Solutions)
                Debug.Log("Cell X: " + solution[0].ToString() +  ", Y: " + solution[1].ToString() + " = " + solution[2].ToString());
        }

        void printSingleElement(string element, string message)
        {
            using (PlQuery queryElement = new PlQuery(element, new PlTermV(new PlTerm("X"))))
            {
                foreach (PlTermV solution in queryElement.Solutions)
                    Debug.Log(message + solution[0].ToString());
            }
        }
    }

    private void initialiseGameKB(int wumpuses, int golds, Coordinates coords)
    {
        PlQuery.PlCall($"initGameAttributes({wumpuses}, {golds})");
        PlQuery.PlCall($"initAgent({coords.x}, {coords.y})");
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
}
